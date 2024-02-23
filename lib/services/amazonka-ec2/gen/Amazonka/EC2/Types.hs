{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors

    -- * Re-exported Types
    module Amazonka.EC2.Internal,

    -- * AcceleratorManufacturer
    AcceleratorManufacturer (..),

    -- * AcceleratorName
    AcceleratorName (..),

    -- * AcceleratorType
    AcceleratorType (..),

    -- * AccountAttributeName
    AccountAttributeName (..),

    -- * ActivityStatus
    ActivityStatus (..),

    -- * AddressAttributeName
    AddressAttributeName (..),

    -- * AddressFamily
    AddressFamily (..),

    -- * AddressStatus
    AddressStatus (..),

    -- * AddressTransferStatus
    AddressTransferStatus (..),

    -- * Affinity
    Affinity (..),

    -- * AllocationState
    AllocationState (..),

    -- * AllocationStrategy
    AllocationStrategy (..),

    -- * AllocationType
    AllocationType (..),

    -- * AllowsMultipleInstanceTypes
    AllowsMultipleInstanceTypes (..),

    -- * AnalysisStatus
    AnalysisStatus (..),

    -- * ApplianceModeSupportValue
    ApplianceModeSupportValue (..),

    -- * ArchitectureType
    ArchitectureType (..),

    -- * ArchitectureValues
    ArchitectureValues (..),

    -- * AssociatedNetworkType
    AssociatedNetworkType (..),

    -- * AssociationStatusCode
    AssociationStatusCode (..),

    -- * AttachmentStatus
    AttachmentStatus (..),

    -- * AutoAcceptSharedAssociationsValue
    AutoAcceptSharedAssociationsValue (..),

    -- * AutoAcceptSharedAttachmentsValue
    AutoAcceptSharedAttachmentsValue (..),

    -- * AutoPlacement
    AutoPlacement (..),

    -- * AvailabilityZoneOptInStatus
    AvailabilityZoneOptInStatus (..),

    -- * AvailabilityZoneState
    AvailabilityZoneState (..),

    -- * BareMetal
    BareMetal (..),

    -- * BatchState
    BatchState (..),

    -- * BgpStatus
    BgpStatus (..),

    -- * BootModeType
    BootModeType (..),

    -- * BootModeValues
    BootModeValues (..),

    -- * BundleTaskState
    BundleTaskState (..),

    -- * BurstablePerformance
    BurstablePerformance (..),

    -- * ByoipCidrState
    ByoipCidrState (..),

    -- * CancelBatchErrorCode
    CancelBatchErrorCode (..),

    -- * CancelSpotInstanceRequestState
    CancelSpotInstanceRequestState (..),

    -- * CapacityReservationFleetState
    CapacityReservationFleetState (..),

    -- * CapacityReservationInstancePlatform
    CapacityReservationInstancePlatform (..),

    -- * CapacityReservationPreference
    CapacityReservationPreference (..),

    -- * CapacityReservationState
    CapacityReservationState (..),

    -- * CapacityReservationTenancy
    CapacityReservationTenancy (..),

    -- * CarrierGatewayState
    CarrierGatewayState (..),

    -- * ClientCertificateRevocationListStatusCode
    ClientCertificateRevocationListStatusCode (..),

    -- * ClientVpnAuthenticationType
    ClientVpnAuthenticationType (..),

    -- * ClientVpnAuthorizationRuleStatusCode
    ClientVpnAuthorizationRuleStatusCode (..),

    -- * ClientVpnConnectionStatusCode
    ClientVpnConnectionStatusCode (..),

    -- * ClientVpnEndpointAttributeStatusCode
    ClientVpnEndpointAttributeStatusCode (..),

    -- * ClientVpnEndpointStatusCode
    ClientVpnEndpointStatusCode (..),

    -- * ClientVpnRouteStatusCode
    ClientVpnRouteStatusCode (..),

    -- * ConnectionNotificationState
    ConnectionNotificationState (..),

    -- * ConnectionNotificationType
    ConnectionNotificationType (..),

    -- * ConnectivityType
    ConnectivityType (..),

    -- * ContainerFormat
    ContainerFormat (..),

    -- * ConversionTaskState
    ConversionTaskState (..),

    -- * CopyTagsFromSource
    CopyTagsFromSource (..),

    -- * CpuManufacturer
    CpuManufacturer (..),

    -- * CurrencyCodeValues
    CurrencyCodeValues (..),

    -- * DatafeedSubscriptionState
    DatafeedSubscriptionState (..),

    -- * DefaultRouteTableAssociationValue
    DefaultRouteTableAssociationValue (..),

    -- * DefaultRouteTablePropagationValue
    DefaultRouteTablePropagationValue (..),

    -- * DefaultTargetCapacityType
    DefaultTargetCapacityType (..),

    -- * DeleteFleetErrorCode
    DeleteFleetErrorCode (..),

    -- * DeleteQueuedReservedInstancesErrorCode
    DeleteQueuedReservedInstancesErrorCode (..),

    -- * DestinationFileFormat
    DestinationFileFormat (..),

    -- * DeviceTrustProviderType
    DeviceTrustProviderType (..),

    -- * DeviceType
    DeviceType (..),

    -- * DiskImageFormat
    DiskImageFormat (..),

    -- * DiskType
    DiskType (..),

    -- * DnsNameState
    DnsNameState (..),

    -- * DnsRecordIpType
    DnsRecordIpType (..),

    -- * DnsSupportValue
    DnsSupportValue (..),

    -- * DomainType
    DomainType (..),

    -- * DynamicRoutingValue
    DynamicRoutingValue (..),

    -- * EbsEncryptionSupport
    EbsEncryptionSupport (..),

    -- * EbsNvmeSupport
    EbsNvmeSupport (..),

    -- * EbsOptimizedSupport
    EbsOptimizedSupport (..),

    -- * ElasticGpuState
    ElasticGpuState (..),

    -- * ElasticGpuStatus
    ElasticGpuStatus (..),

    -- * EnaSupport
    EnaSupport (..),

    -- * EndDateType
    EndDateType (..),

    -- * EphemeralNvmeSupport
    EphemeralNvmeSupport (..),

    -- * EventCode
    EventCode (..),

    -- * EventType
    EventType (..),

    -- * ExcessCapacityTerminationPolicy
    ExcessCapacityTerminationPolicy (..),

    -- * ExportEnvironment
    ExportEnvironment (..),

    -- * ExportTaskState
    ExportTaskState (..),

    -- * FastLaunchResourceType
    FastLaunchResourceType (..),

    -- * FastLaunchStateCode
    FastLaunchStateCode (..),

    -- * FastSnapshotRestoreStateCode
    FastSnapshotRestoreStateCode (..),

    -- * FindingsFound
    FindingsFound (..),

    -- * FleetActivityStatus
    FleetActivityStatus (..),

    -- * FleetCapacityReservationTenancy
    FleetCapacityReservationTenancy (..),

    -- * FleetCapacityReservationUsageStrategy
    FleetCapacityReservationUsageStrategy (..),

    -- * FleetEventType
    FleetEventType (..),

    -- * FleetExcessCapacityTerminationPolicy
    FleetExcessCapacityTerminationPolicy (..),

    -- * FleetInstanceMatchCriteria
    FleetInstanceMatchCriteria (..),

    -- * FleetOnDemandAllocationStrategy
    FleetOnDemandAllocationStrategy (..),

    -- * FleetReplacementStrategy
    FleetReplacementStrategy (..),

    -- * FleetStateCode
    FleetStateCode (..),

    -- * FleetType
    FleetType (..),

    -- * FlowLogsResourceType
    FlowLogsResourceType (..),

    -- * FpgaImageAttributeName
    FpgaImageAttributeName (..),

    -- * FpgaImageStateCode
    FpgaImageStateCode (..),

    -- * GatewayAssociationState
    GatewayAssociationState (..),

    -- * GatewayType
    GatewayType (..),

    -- * HostRecovery
    HostRecovery (..),

    -- * HostTenancy
    HostTenancy (..),

    -- * HostnameType
    HostnameType (..),

    -- * HttpTokensState
    HttpTokensState (..),

    -- * HypervisorType
    HypervisorType (..),

    -- * IamInstanceProfileAssociationState
    IamInstanceProfileAssociationState (..),

    -- * Igmpv2SupportValue
    Igmpv2SupportValue (..),

    -- * ImageAttributeName
    ImageAttributeName (..),

    -- * ImageState
    ImageState (..),

    -- * ImageTypeValues
    ImageTypeValues (..),

    -- * ImdsSupportValues
    ImdsSupportValues (..),

    -- * InstanceAttributeName
    InstanceAttributeName (..),

    -- * InstanceAutoRecoveryState
    InstanceAutoRecoveryState (..),

    -- * InstanceEventWindowState
    InstanceEventWindowState (..),

    -- * InstanceGeneration
    InstanceGeneration (..),

    -- * InstanceHealthStatus
    InstanceHealthStatus (..),

    -- * InstanceInterruptionBehavior
    InstanceInterruptionBehavior (..),

    -- * InstanceLifecycle
    InstanceLifecycle (..),

    -- * InstanceLifecycleType
    InstanceLifecycleType (..),

    -- * InstanceMatchCriteria
    InstanceMatchCriteria (..),

    -- * InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- * InstanceMetadataOptionsState
    InstanceMetadataOptionsState (..),

    -- * InstanceMetadataProtocolState
    InstanceMetadataProtocolState (..),

    -- * InstanceMetadataTagsState
    InstanceMetadataTagsState (..),

    -- * InstanceStateName
    InstanceStateName (..),

    -- * InstanceStorageEncryptionSupport
    InstanceStorageEncryptionSupport (..),

    -- * InstanceType
    InstanceType (..),

    -- * InstanceTypeHypervisor
    InstanceTypeHypervisor (..),

    -- * InterfacePermissionType
    InterfacePermissionType (..),

    -- * InterfaceProtocolType
    InterfaceProtocolType (..),

    -- * IpAddressType
    IpAddressType (..),

    -- * IpamAddressHistoryResourceType
    IpamAddressHistoryResourceType (..),

    -- * IpamComplianceStatus
    IpamComplianceStatus (..),

    -- * IpamManagementState
    IpamManagementState (..),

    -- * IpamOverlapStatus
    IpamOverlapStatus (..),

    -- * IpamPoolAllocationResourceType
    IpamPoolAllocationResourceType (..),

    -- * IpamPoolAwsService
    IpamPoolAwsService (..),

    -- * IpamPoolCidrFailureCode
    IpamPoolCidrFailureCode (..),

    -- * IpamPoolCidrState
    IpamPoolCidrState (..),

    -- * IpamPoolState
    IpamPoolState (..),

    -- * IpamResourceType
    IpamResourceType (..),

    -- * IpamScopeState
    IpamScopeState (..),

    -- * IpamScopeType
    IpamScopeType (..),

    -- * IpamState
    IpamState (..),

    -- * Ipv6SupportValue
    Ipv6SupportValue (..),

    -- * KeyFormat
    KeyFormat (..),

    -- * KeyType
    KeyType (..),

    -- * LaunchTemplateAutoRecoveryState
    LaunchTemplateAutoRecoveryState (..),

    -- * LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- * LaunchTemplateHttpTokensState
    LaunchTemplateHttpTokensState (..),

    -- * LaunchTemplateInstanceMetadataEndpointState
    LaunchTemplateInstanceMetadataEndpointState (..),

    -- * LaunchTemplateInstanceMetadataOptionsState
    LaunchTemplateInstanceMetadataOptionsState (..),

    -- * LaunchTemplateInstanceMetadataProtocolIpv6
    LaunchTemplateInstanceMetadataProtocolIpv6 (..),

    -- * LaunchTemplateInstanceMetadataTagsState
    LaunchTemplateInstanceMetadataTagsState (..),

    -- * ListingState
    ListingState (..),

    -- * ListingStatus
    ListingStatus (..),

    -- * LocalGatewayRouteState
    LocalGatewayRouteState (..),

    -- * LocalGatewayRouteTableMode
    LocalGatewayRouteTableMode (..),

    -- * LocalGatewayRouteType
    LocalGatewayRouteType (..),

    -- * LocalStorage
    LocalStorage (..),

    -- * LocalStorageType
    LocalStorageType (..),

    -- * LocationType
    LocationType (..),

    -- * LogDestinationType
    LogDestinationType (..),

    -- * MarketType
    MarketType (..),

    -- * MembershipType
    MembershipType (..),

    -- * MetricType
    MetricType (..),

    -- * ModifyAvailabilityZoneOptInStatus
    ModifyAvailabilityZoneOptInStatus (..),

    -- * MonitoringState
    MonitoringState (..),

    -- * MoveStatus
    MoveStatus (..),

    -- * MulticastSupportValue
    MulticastSupportValue (..),

    -- * NatGatewayState
    NatGatewayState (..),

    -- * NetworkInterfaceAttribute
    NetworkInterfaceAttribute (..),

    -- * NetworkInterfaceCreationType
    NetworkInterfaceCreationType (..),

    -- * NetworkInterfacePermissionStateCode
    NetworkInterfacePermissionStateCode (..),

    -- * NetworkInterfaceStatus
    NetworkInterfaceStatus (..),

    -- * NetworkInterfaceType
    NetworkInterfaceType (..),

    -- * OfferingClassType
    OfferingClassType (..),

    -- * OfferingTypeValues
    OfferingTypeValues (..),

    -- * OnDemandAllocationStrategy
    OnDemandAllocationStrategy (..),

    -- * OperationType
    OperationType (..),

    -- * PartitionLoadFrequency
    PartitionLoadFrequency (..),

    -- * PayerResponsibility
    PayerResponsibility (..),

    -- * PaymentOption
    PaymentOption (..),

    -- * PeriodType
    PeriodType (..),

    -- * PermissionGroup
    PermissionGroup (..),

    -- * PlacementGroupState
    PlacementGroupState (..),

    -- * PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- * PlacementStrategy
    PlacementStrategy (..),

    -- * PlatformValues
    PlatformValues (..),

    -- * PrefixListState
    PrefixListState (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * ProductCodeValues
    ProductCodeValues (..),

    -- * Protocol
    Protocol (..),

    -- * ProtocolValue
    ProtocolValue (..),

    -- * RIProductDescription
    RIProductDescription (..),

    -- * RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- * ReplaceRootVolumeTaskState
    ReplaceRootVolumeTaskState (..),

    -- * ReplacementStrategy
    ReplacementStrategy (..),

    -- * ReportInstanceReasonCodes
    ReportInstanceReasonCodes (..),

    -- * ReportStatusType
    ReportStatusType (..),

    -- * ReservationState
    ReservationState (..),

    -- * ReservedInstanceState
    ReservedInstanceState (..),

    -- * ResetFpgaImageAttributeName
    ResetFpgaImageAttributeName (..),

    -- * ResetImageAttributeName
    ResetImageAttributeName (..),

    -- * ResourceType
    ResourceType (..),

    -- * RootDeviceType
    RootDeviceType (..),

    -- * RouteOrigin
    RouteOrigin (..),

    -- * RouteState
    RouteState (..),

    -- * RouteTableAssociationStateCode
    RouteTableAssociationStateCode (..),

    -- * RuleAction
    RuleAction (..),

    -- * Scope
    Scope (..),

    -- * SelfServicePortal
    SelfServicePortal (..),

    -- * ServiceConnectivityType
    ServiceConnectivityType (..),

    -- * ServiceState
    ServiceState (..),

    -- * ServiceType
    ServiceType (..),

    -- * ShutdownBehavior
    ShutdownBehavior (..),

    -- * SnapshotAttributeName
    SnapshotAttributeName (..),

    -- * SnapshotState
    SnapshotState (..),

    -- * SpotAllocationStrategy
    SpotAllocationStrategy (..),

    -- * SpotInstanceInterruptionBehavior
    SpotInstanceInterruptionBehavior (..),

    -- * SpotInstanceState
    SpotInstanceState (..),

    -- * SpotInstanceType
    SpotInstanceType (..),

    -- * SpreadLevel
    SpreadLevel (..),

    -- * State
    State (..),

    -- * StaticSourcesSupportValue
    StaticSourcesSupportValue (..),

    -- * StatisticType
    StatisticType (..),

    -- * StatusName
    StatusName (..),

    -- * StatusType
    StatusType (..),

    -- * StorageTier
    StorageTier (..),

    -- * SubnetCidrBlockStateCode
    SubnetCidrBlockStateCode (..),

    -- * SubnetCidrReservationType
    SubnetCidrReservationType (..),

    -- * SubnetState
    SubnetState (..),

    -- * SummaryStatus
    SummaryStatus (..),

    -- * TargetCapacityUnitType
    TargetCapacityUnitType (..),

    -- * TargetStorageTier
    TargetStorageTier (..),

    -- * TelemetryStatus
    TelemetryStatus (..),

    -- * Tenancy
    Tenancy (..),

    -- * TieringOperationStatus
    TieringOperationStatus (..),

    -- * TpmSupportValues
    TpmSupportValues (..),

    -- * TrafficDirection
    TrafficDirection (..),

    -- * TrafficMirrorFilterRuleField
    TrafficMirrorFilterRuleField (..),

    -- * TrafficMirrorNetworkService
    TrafficMirrorNetworkService (..),

    -- * TrafficMirrorRuleAction
    TrafficMirrorRuleAction (..),

    -- * TrafficMirrorSessionField
    TrafficMirrorSessionField (..),

    -- * TrafficMirrorTargetType
    TrafficMirrorTargetType (..),

    -- * TrafficType
    TrafficType (..),

    -- * TransitGatewayAssociationState
    TransitGatewayAssociationState (..),

    -- * TransitGatewayAttachmentResourceType
    TransitGatewayAttachmentResourceType (..),

    -- * TransitGatewayAttachmentState
    TransitGatewayAttachmentState (..),

    -- * TransitGatewayConnectPeerState
    TransitGatewayConnectPeerState (..),

    -- * TransitGatewayMulitcastDomainAssociationState
    TransitGatewayMulitcastDomainAssociationState (..),

    -- * TransitGatewayMulticastDomainState
    TransitGatewayMulticastDomainState (..),

    -- * TransitGatewayPolicyTableState
    TransitGatewayPolicyTableState (..),

    -- * TransitGatewayPrefixListReferenceState
    TransitGatewayPrefixListReferenceState (..),

    -- * TransitGatewayPropagationState
    TransitGatewayPropagationState (..),

    -- * TransitGatewayRouteState
    TransitGatewayRouteState (..),

    -- * TransitGatewayRouteTableAnnouncementDirection
    TransitGatewayRouteTableAnnouncementDirection (..),

    -- * TransitGatewayRouteTableAnnouncementState
    TransitGatewayRouteTableAnnouncementState (..),

    -- * TransitGatewayRouteTableState
    TransitGatewayRouteTableState (..),

    -- * TransitGatewayRouteType
    TransitGatewayRouteType (..),

    -- * TransitGatewayState
    TransitGatewayState (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * TrustProviderType
    TrustProviderType (..),

    -- * TunnelInsideIpVersion
    TunnelInsideIpVersion (..),

    -- * UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- * UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- * UsageClassType
    UsageClassType (..),

    -- * UserTrustProviderType
    UserTrustProviderType (..),

    -- * VerifiedAccessEndpointAttachmentType
    VerifiedAccessEndpointAttachmentType (..),

    -- * VerifiedAccessEndpointProtocol
    VerifiedAccessEndpointProtocol (..),

    -- * VerifiedAccessEndpointStatusCode
    VerifiedAccessEndpointStatusCode (..),

    -- * VerifiedAccessEndpointType
    VerifiedAccessEndpointType (..),

    -- * VerifiedAccessLogDeliveryStatusCode
    VerifiedAccessLogDeliveryStatusCode (..),

    -- * VirtualizationType
    VirtualizationType (..),

    -- * VolumeAttachmentState
    VolumeAttachmentState (..),

    -- * VolumeAttributeName
    VolumeAttributeName (..),

    -- * VolumeModificationState
    VolumeModificationState (..),

    -- * VolumeState
    VolumeState (..),

    -- * VolumeStatusInfoStatus
    VolumeStatusInfoStatus (..),

    -- * VolumeStatusName
    VolumeStatusName (..),

    -- * VolumeType
    VolumeType (..),

    -- * VpcAttributeName
    VpcAttributeName (..),

    -- * VpcCidrBlockStateCode
    VpcCidrBlockStateCode (..),

    -- * VpcEndpointType
    VpcEndpointType (..),

    -- * VpcPeeringConnectionStateReasonCode
    VpcPeeringConnectionStateReasonCode (..),

    -- * VpcState
    VpcState (..),

    -- * VpcTenancy
    VpcTenancy (..),

    -- * VpnEcmpSupportValue
    VpnEcmpSupportValue (..),

    -- * VpnProtocol
    VpnProtocol (..),

    -- * VpnState
    VpnState (..),

    -- * VpnStaticRouteSource
    VpnStaticRouteSource (..),

    -- * WeekDay
    WeekDay (..),

    -- * AcceleratorCount
    AcceleratorCount (..),
    newAcceleratorCount,
    acceleratorCount_max,
    acceleratorCount_min,

    -- * AcceleratorCountRequest
    AcceleratorCountRequest (..),
    newAcceleratorCountRequest,
    acceleratorCountRequest_max,
    acceleratorCountRequest_min,

    -- * AcceleratorTotalMemoryMiB
    AcceleratorTotalMemoryMiB (..),
    newAcceleratorTotalMemoryMiB,
    acceleratorTotalMemoryMiB_max,
    acceleratorTotalMemoryMiB_min,

    -- * AcceleratorTotalMemoryMiBRequest
    AcceleratorTotalMemoryMiBRequest (..),
    newAcceleratorTotalMemoryMiBRequest,
    acceleratorTotalMemoryMiBRequest_max,
    acceleratorTotalMemoryMiBRequest_min,

    -- * AccessScopeAnalysisFinding
    AccessScopeAnalysisFinding (..),
    newAccessScopeAnalysisFinding,
    accessScopeAnalysisFinding_findingComponents,
    accessScopeAnalysisFinding_findingId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeId,

    -- * AccessScopePath
    AccessScopePath (..),
    newAccessScopePath,
    accessScopePath_destination,
    accessScopePath_source,
    accessScopePath_throughResources,

    -- * AccessScopePathRequest
    AccessScopePathRequest (..),
    newAccessScopePathRequest,
    accessScopePathRequest_destination,
    accessScopePathRequest_source,
    accessScopePathRequest_throughResources,

    -- * AccountAttribute
    AccountAttribute (..),
    newAccountAttribute,
    accountAttribute_attributeName,
    accountAttribute_attributeValues,

    -- * AccountAttributeValue
    AccountAttributeValue (..),
    newAccountAttributeValue,
    accountAttributeValue_attributeValue,

    -- * ActiveInstance
    ActiveInstance (..),
    newActiveInstance,
    activeInstance_instanceHealth,
    activeInstance_instanceId,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,

    -- * AddIpamOperatingRegion
    AddIpamOperatingRegion (..),
    newAddIpamOperatingRegion,
    addIpamOperatingRegion_regionName,

    -- * AddPrefixListEntry
    AddPrefixListEntry (..),
    newAddPrefixListEntry,
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- * AddedPrincipal
    AddedPrincipal (..),
    newAddedPrincipal,
    addedPrincipal_principal,
    addedPrincipal_principalType,
    addedPrincipal_serviceId,
    addedPrincipal_servicePermissionId,

    -- * AdditionalDetail
    AdditionalDetail (..),
    newAdditionalDetail,
    additionalDetail_additionalDetailType,
    additionalDetail_component,

    -- * Address
    Address (..),
    newAddress,
    address_allocationId,
    address_associationId,
    address_carrierIp,
    address_customerOwnedIp,
    address_customerOwnedIpv4Pool,
    address_domain,
    address_instanceId,
    address_networkBorderGroup,
    address_networkInterfaceId,
    address_networkInterfaceOwnerId,
    address_privateIpAddress,
    address_publicIp,
    address_publicIpv4Pool,
    address_tags,

    -- * AddressAttribute
    AddressAttribute (..),
    newAddressAttribute,
    addressAttribute_allocationId,
    addressAttribute_ptrRecord,
    addressAttribute_ptrRecordUpdate,
    addressAttribute_publicIp,

    -- * AddressTransfer
    AddressTransfer (..),
    newAddressTransfer,
    addressTransfer_addressTransferStatus,
    addressTransfer_allocationId,
    addressTransfer_publicIp,
    addressTransfer_transferAccountId,
    addressTransfer_transferOfferAcceptedTimestamp,
    addressTransfer_transferOfferExpirationTimestamp,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    newAllowedPrincipal,
    allowedPrincipal_principal,
    allowedPrincipal_principalType,
    allowedPrincipal_serviceId,
    allowedPrincipal_servicePermissionId,
    allowedPrincipal_tags,

    -- * AlternatePathHint
    AlternatePathHint (..),
    newAlternatePathHint,
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- * AnalysisAclRule
    AnalysisAclRule (..),
    newAnalysisAclRule,
    analysisAclRule_cidr,
    analysisAclRule_egress,
    analysisAclRule_portRange,
    analysisAclRule_protocol,
    analysisAclRule_ruleAction,
    analysisAclRule_ruleNumber,

    -- * AnalysisComponent
    AnalysisComponent (..),
    newAnalysisComponent,
    analysisComponent_arn,
    analysisComponent_id,
    analysisComponent_name,

    -- * AnalysisLoadBalancerListener
    AnalysisLoadBalancerListener (..),
    newAnalysisLoadBalancerListener,
    analysisLoadBalancerListener_instancePort,
    analysisLoadBalancerListener_loadBalancerPort,

    -- * AnalysisLoadBalancerTarget
    AnalysisLoadBalancerTarget (..),
    newAnalysisLoadBalancerTarget,
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_instance,
    analysisLoadBalancerTarget_port,

    -- * AnalysisPacketHeader
    AnalysisPacketHeader (..),
    newAnalysisPacketHeader,
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_protocol,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_sourcePortRanges,

    -- * AnalysisRouteTableRoute
    AnalysisRouteTableRoute (..),
    newAnalysisRouteTableRoute,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_state,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_vpcPeeringConnectionId,

    -- * AnalysisSecurityGroupRule
    AnalysisSecurityGroupRule (..),
    newAnalysisSecurityGroupRule,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_securityGroupId,

    -- * AssignedPrivateIpAddress
    AssignedPrivateIpAddress (..),
    newAssignedPrivateIpAddress,
    assignedPrivateIpAddress_privateIpAddress,

    -- * AssociatedRole
    AssociatedRole (..),
    newAssociatedRole,
    associatedRole_associatedRoleArn,
    associatedRole_certificateS3BucketName,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,

    -- * AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    newAssociatedTargetNetwork,
    associatedTargetNetwork_networkId,
    associatedTargetNetwork_networkType,

    -- * AssociationStatus
    AssociationStatus (..),
    newAssociationStatus,
    associationStatus_code,
    associationStatus_message,

    -- * AthenaIntegration
    AthenaIntegration (..),
    newAthenaIntegration,
    athenaIntegration_partitionEndDate,
    athenaIntegration_partitionStartDate,
    athenaIntegration_integrationResultS3DestinationArn,
    athenaIntegration_partitionLoadFrequency,

    -- * AttachmentEnaSrdSpecification
    AttachmentEnaSrdSpecification (..),
    newAttachmentEnaSrdSpecification,
    attachmentEnaSrdSpecification_enaSrdEnabled,
    attachmentEnaSrdSpecification_enaSrdUdpSpecification,

    -- * AttachmentEnaSrdUdpSpecification
    AttachmentEnaSrdUdpSpecification (..),
    newAttachmentEnaSrdUdpSpecification,
    attachmentEnaSrdUdpSpecification_enaSrdUdpEnabled,

    -- * AttributeBooleanValue
    AttributeBooleanValue (..),
    newAttributeBooleanValue,
    attributeBooleanValue_value,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_value,

    -- * AuthorizationRule
    AuthorizationRule (..),
    newAuthorizationRule,
    authorizationRule_accessAll,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_description,
    authorizationRule_destinationCidr,
    authorizationRule_groupId,
    authorizationRule_status,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_groupName,
    availabilityZone_messages,
    availabilityZone_networkBorderGroup,
    availabilityZone_optInStatus,
    availabilityZone_parentZoneId,
    availabilityZone_parentZoneName,
    availabilityZone_regionName,
    availabilityZone_state,
    availabilityZone_zoneId,
    availabilityZone_zoneName,
    availabilityZone_zoneType,

    -- * AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    newAvailabilityZoneMessage,
    availabilityZoneMessage_message,

    -- * AvailableCapacity
    AvailableCapacity (..),
    newAvailableCapacity,
    availableCapacity_availableInstanceCapacity,
    availableCapacity_availableVCpus,

    -- * BaselineEbsBandwidthMbps
    BaselineEbsBandwidthMbps (..),
    newBaselineEbsBandwidthMbps,
    baselineEbsBandwidthMbps_max,
    baselineEbsBandwidthMbps_min,

    -- * BaselineEbsBandwidthMbpsRequest
    BaselineEbsBandwidthMbpsRequest (..),
    newBaselineEbsBandwidthMbpsRequest,
    baselineEbsBandwidthMbpsRequest_max,
    baselineEbsBandwidthMbpsRequest_min,

    -- * BlobAttributeValue
    BlobAttributeValue (..),
    newBlobAttributeValue,
    blobAttributeValue_value,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- * BundleTask
    BundleTask (..),
    newBundleTask,
    bundleTask_bundleTaskError,
    bundleTask_bundleId,
    bundleTask_instanceId,
    bundleTask_progress,
    bundleTask_startTime,
    bundleTask_state,
    bundleTask_storage,
    bundleTask_updateTime,

    -- * BundleTaskError
    BundleTaskError (..),
    newBundleTaskError,
    bundleTaskError_code,
    bundleTaskError_message,

    -- * ByoipCidr
    ByoipCidr (..),
    newByoipCidr,
    byoipCidr_cidr,
    byoipCidr_description,
    byoipCidr_state,
    byoipCidr_statusMessage,

    -- * CancelCapacityReservationFleetError
    CancelCapacityReservationFleetError (..),
    newCancelCapacityReservationFleetError,
    cancelCapacityReservationFleetError_code,
    cancelCapacityReservationFleetError_message,

    -- * CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    newCancelSpotFleetRequestsError,
    cancelSpotFleetRequestsError_code,
    cancelSpotFleetRequestsError_message,

    -- * CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (..),
    newCancelSpotFleetRequestsErrorItem,
    cancelSpotFleetRequestsErrorItem_error,
    cancelSpotFleetRequestsErrorItem_spotFleetRequestId,

    -- * CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (..),
    newCancelSpotFleetRequestsSuccessItem,
    cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,

    -- * CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    newCancelledSpotInstanceRequest,
    cancelledSpotInstanceRequest_spotInstanceRequestId,
    cancelledSpotInstanceRequest_state,

    -- * CapacityAllocation
    CapacityAllocation (..),
    newCapacityAllocation,
    capacityAllocation_allocationType,
    capacityAllocation_count,

    -- * CapacityReservation
    CapacityReservation (..),
    newCapacityReservation,
    capacityReservation_availabilityZone,
    capacityReservation_availabilityZoneId,
    capacityReservation_availableInstanceCount,
    capacityReservation_capacityAllocations,
    capacityReservation_capacityReservationArn,
    capacityReservation_capacityReservationFleetId,
    capacityReservation_capacityReservationId,
    capacityReservation_createDate,
    capacityReservation_ebsOptimized,
    capacityReservation_endDate,
    capacityReservation_endDateType,
    capacityReservation_ephemeralStorage,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_instancePlatform,
    capacityReservation_instanceType,
    capacityReservation_outpostArn,
    capacityReservation_ownerId,
    capacityReservation_placementGroupArn,
    capacityReservation_startDate,
    capacityReservation_state,
    capacityReservation_tags,
    capacityReservation_tenancy,
    capacityReservation_totalInstanceCount,

    -- * CapacityReservationFleet
    CapacityReservationFleet (..),
    newCapacityReservationFleet,
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_createTime,
    capacityReservationFleet_endDate,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_state,
    capacityReservationFleet_tags,
    capacityReservationFleet_tenancy,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_totalTargetCapacity,

    -- * CapacityReservationFleetCancellationState
    CapacityReservationFleetCancellationState (..),
    newCapacityReservationFleetCancellationState,
    capacityReservationFleetCancellationState_capacityReservationFleetId,
    capacityReservationFleetCancellationState_currentFleetState,
    capacityReservationFleetCancellationState_previousFleetState,

    -- * CapacityReservationGroup
    CapacityReservationGroup (..),
    newCapacityReservationGroup,
    capacityReservationGroup_groupArn,
    capacityReservationGroup_ownerId,

    -- * CapacityReservationOptions
    CapacityReservationOptions (..),
    newCapacityReservationOptions,
    capacityReservationOptions_usageStrategy,

    -- * CapacityReservationOptionsRequest
    CapacityReservationOptionsRequest (..),
    newCapacityReservationOptionsRequest,
    capacityReservationOptionsRequest_usageStrategy,

    -- * CapacityReservationSpecification
    CapacityReservationSpecification (..),
    newCapacityReservationSpecification,
    capacityReservationSpecification_capacityReservationPreference,
    capacityReservationSpecification_capacityReservationTarget,

    -- * CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (..),
    newCapacityReservationSpecificationResponse,
    capacityReservationSpecificationResponse_capacityReservationPreference,
    capacityReservationSpecificationResponse_capacityReservationTarget,

    -- * CapacityReservationTarget
    CapacityReservationTarget (..),
    newCapacityReservationTarget,
    capacityReservationTarget_capacityReservationId,
    capacityReservationTarget_capacityReservationResourceGroupArn,

    -- * CapacityReservationTargetResponse
    CapacityReservationTargetResponse (..),
    newCapacityReservationTargetResponse,
    capacityReservationTargetResponse_capacityReservationId,
    capacityReservationTargetResponse_capacityReservationResourceGroupArn,

    -- * CarrierGateway
    CarrierGateway (..),
    newCarrierGateway,
    carrierGateway_carrierGatewayId,
    carrierGateway_ownerId,
    carrierGateway_state,
    carrierGateway_tags,
    carrierGateway_vpcId,

    -- * CertificateAuthentication
    CertificateAuthentication (..),
    newCertificateAuthentication,
    certificateAuthentication_clientRootCertificateChain,

    -- * CertificateAuthenticationRequest
    CertificateAuthenticationRequest (..),
    newCertificateAuthenticationRequest,
    certificateAuthenticationRequest_clientRootCertificateChainArn,

    -- * CidrAuthorizationContext
    CidrAuthorizationContext (..),
    newCidrAuthorizationContext,
    cidrAuthorizationContext_message,
    cidrAuthorizationContext_signature,

    -- * CidrBlock
    CidrBlock (..),
    newCidrBlock,
    cidrBlock_cidrBlock,

    -- * ClassicLinkDnsSupport
    ClassicLinkDnsSupport (..),
    newClassicLinkDnsSupport,
    classicLinkDnsSupport_classicLinkDnsSupported,
    classicLinkDnsSupport_vpcId,

    -- * ClassicLinkInstance
    ClassicLinkInstance (..),
    newClassicLinkInstance,
    classicLinkInstance_groups,
    classicLinkInstance_instanceId,
    classicLinkInstance_tags,
    classicLinkInstance_vpcId,

    -- * ClassicLoadBalancer
    ClassicLoadBalancer (..),
    newClassicLoadBalancer,
    classicLoadBalancer_name,

    -- * ClassicLoadBalancersConfig
    ClassicLoadBalancersConfig (..),
    newClassicLoadBalancersConfig,
    classicLoadBalancersConfig_classicLoadBalancers,

    -- * ClientCertificateRevocationListStatus
    ClientCertificateRevocationListStatus (..),
    newClientCertificateRevocationListStatus,
    clientCertificateRevocationListStatus_code,
    clientCertificateRevocationListStatus_message,

    -- * ClientConnectOptions
    ClientConnectOptions (..),
    newClientConnectOptions,
    clientConnectOptions_enabled,
    clientConnectOptions_lambdaFunctionArn,

    -- * ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    newClientConnectResponseOptions,
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,
    clientConnectResponseOptions_status,

    -- * ClientData
    ClientData (..),
    newClientData,
    clientData_comment,
    clientData_uploadEnd,
    clientData_uploadSize,
    clientData_uploadStart,

    -- * ClientLoginBannerOptions
    ClientLoginBannerOptions (..),
    newClientLoginBannerOptions,
    clientLoginBannerOptions_bannerText,
    clientLoginBannerOptions_enabled,

    -- * ClientLoginBannerResponseOptions
    ClientLoginBannerResponseOptions (..),
    newClientLoginBannerResponseOptions,
    clientLoginBannerResponseOptions_bannerText,
    clientLoginBannerResponseOptions_enabled,

    -- * ClientVpnAuthentication
    ClientVpnAuthentication (..),
    newClientVpnAuthentication,
    clientVpnAuthentication_activeDirectory,
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_type,

    -- * ClientVpnAuthenticationRequest
    ClientVpnAuthenticationRequest (..),
    newClientVpnAuthenticationRequest,
    clientVpnAuthenticationRequest_activeDirectory,
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_type,

    -- * ClientVpnAuthorizationRuleStatus
    ClientVpnAuthorizationRuleStatus (..),
    newClientVpnAuthorizationRuleStatus,
    clientVpnAuthorizationRuleStatus_code,
    clientVpnAuthorizationRuleStatus_message,

    -- * ClientVpnConnection
    ClientVpnConnection (..),
    newClientVpnConnection,
    clientVpnConnection_clientIp,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_connectionId,
    clientVpnConnection_egressBytes,
    clientVpnConnection_egressPackets,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_status,
    clientVpnConnection_timestamp,
    clientVpnConnection_username,

    -- * ClientVpnConnectionStatus
    ClientVpnConnectionStatus (..),
    newClientVpnConnectionStatus,
    clientVpnConnectionStatus_code,
    clientVpnConnectionStatus_message,

    -- * ClientVpnEndpoint
    ClientVpnEndpoint (..),
    newClientVpnEndpoint,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_clientLoginBannerOptions,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_description,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_sessionTimeoutHours,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_status,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_vpnProtocol,

    -- * ClientVpnEndpointAttributeStatus
    ClientVpnEndpointAttributeStatus (..),
    newClientVpnEndpointAttributeStatus,
    clientVpnEndpointAttributeStatus_code,
    clientVpnEndpointAttributeStatus_message,

    -- * ClientVpnEndpointStatus
    ClientVpnEndpointStatus (..),
    newClientVpnEndpointStatus,
    clientVpnEndpointStatus_code,
    clientVpnEndpointStatus_message,

    -- * ClientVpnRoute
    ClientVpnRoute (..),
    newClientVpnRoute,
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_description,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_origin,
    clientVpnRoute_status,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_type,

    -- * ClientVpnRouteStatus
    ClientVpnRouteStatus (..),
    newClientVpnRouteStatus,
    clientVpnRouteStatus_code,
    clientVpnRouteStatus_message,

    -- * CloudWatchLogOptions
    CloudWatchLogOptions (..),
    newCloudWatchLogOptions,
    cloudWatchLogOptions_logEnabled,
    cloudWatchLogOptions_logGroupArn,
    cloudWatchLogOptions_logOutputFormat,

    -- * CloudWatchLogOptionsSpecification
    CloudWatchLogOptionsSpecification (..),
    newCloudWatchLogOptionsSpecification,
    cloudWatchLogOptionsSpecification_logEnabled,
    cloudWatchLogOptionsSpecification_logGroupArn,
    cloudWatchLogOptionsSpecification_logOutputFormat,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    newCoipAddressUsage,
    coipAddressUsage_allocationId,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_awsService,
    coipAddressUsage_coIp,

    -- * CoipCidr
    CoipCidr (..),
    newCoipCidr,
    coipCidr_cidr,
    coipCidr_coipPoolId,
    coipCidr_localGatewayRouteTableId,

    -- * CoipPool
    CoipPool (..),
    newCoipPool,
    coipPool_localGatewayRouteTableId,
    coipPool_poolArn,
    coipPool_poolCidrs,
    coipPool_poolId,
    coipPool_tags,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    newConnectionLogOptions,
    connectionLogOptions_cloudwatchLogGroup,
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_enabled,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    newConnectionLogResponseOptions,
    connectionLogResponseOptions_cloudwatchLogGroup,
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_enabled,

    -- * ConnectionNotification
    ConnectionNotification (..),
    newConnectionNotification,
    connectionNotification_connectionEvents,
    connectionNotification_connectionNotificationArn,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationType,
    connectionNotification_serviceId,
    connectionNotification_vpcEndpointId,

    -- * ConversionTask
    ConversionTask (..),
    newConversionTask,
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
    conversionTask_importInstance,
    conversionTask_importVolume,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_tags,

    -- * CpuOptions
    CpuOptions (..),
    newCpuOptions,
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- * CpuOptionsRequest
    CpuOptionsRequest (..),
    newCpuOptionsRequest,
    cpuOptionsRequest_coreCount,
    cpuOptionsRequest_threadsPerCore,

    -- * CreateFleetError
    CreateFleetError (..),
    newCreateFleetError,
    createFleetError_errorCode,
    createFleetError_errorMessage,
    createFleetError_launchTemplateAndOverrides,
    createFleetError_lifecycle,

    -- * CreateFleetInstance
    CreateFleetInstance (..),
    newCreateFleetInstance,
    createFleetInstance_instanceIds,
    createFleetInstance_instanceType,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,
    createFleetInstance_platform,

    -- * CreateTransitGatewayConnectRequestOptions
    CreateTransitGatewayConnectRequestOptions (..),
    newCreateTransitGatewayConnectRequestOptions,
    createTransitGatewayConnectRequestOptions_protocol,

    -- * CreateTransitGatewayMulticastDomainRequestOptions
    CreateTransitGatewayMulticastDomainRequestOptions (..),
    newCreateTransitGatewayMulticastDomainRequestOptions,
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,

    -- * CreateTransitGatewayPeeringAttachmentRequestOptions
    CreateTransitGatewayPeeringAttachmentRequestOptions (..),
    newCreateTransitGatewayPeeringAttachmentRequestOptions,
    createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting,

    -- * CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (..),
    newCreateTransitGatewayVpcAttachmentRequestOptions,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- * CreateVerifiedAccessEndpointEniOptions
    CreateVerifiedAccessEndpointEniOptions (..),
    newCreateVerifiedAccessEndpointEniOptions,
    createVerifiedAccessEndpointEniOptions_networkInterfaceId,
    createVerifiedAccessEndpointEniOptions_port,
    createVerifiedAccessEndpointEniOptions_protocol,

    -- * CreateVerifiedAccessEndpointLoadBalancerOptions
    CreateVerifiedAccessEndpointLoadBalancerOptions (..),
    newCreateVerifiedAccessEndpointLoadBalancerOptions,
    createVerifiedAccessEndpointLoadBalancerOptions_loadBalancerArn,
    createVerifiedAccessEndpointLoadBalancerOptions_port,
    createVerifiedAccessEndpointLoadBalancerOptions_protocol,
    createVerifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- * CreateVerifiedAccessTrustProviderDeviceOptions
    CreateVerifiedAccessTrustProviderDeviceOptions (..),
    newCreateVerifiedAccessTrustProviderDeviceOptions,
    createVerifiedAccessTrustProviderDeviceOptions_tenantId,

    -- * CreateVerifiedAccessTrustProviderOidcOptions
    CreateVerifiedAccessTrustProviderOidcOptions (..),
    newCreateVerifiedAccessTrustProviderOidcOptions,
    createVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint,
    createVerifiedAccessTrustProviderOidcOptions_clientId,
    createVerifiedAccessTrustProviderOidcOptions_clientSecret,
    createVerifiedAccessTrustProviderOidcOptions_issuer,
    createVerifiedAccessTrustProviderOidcOptions_scope,
    createVerifiedAccessTrustProviderOidcOptions_tokenEndpoint,
    createVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint,

    -- * CreateVolumePermission
    CreateVolumePermission (..),
    newCreateVolumePermission,
    createVolumePermission_group,
    createVolumePermission_userId,

    -- * CreateVolumePermissionModifications
    CreateVolumePermissionModifications (..),
    newCreateVolumePermissionModifications,
    createVolumePermissionModifications_add,
    createVolumePermissionModifications_remove,

    -- * CreditSpecification
    CreditSpecification (..),
    newCreditSpecification,
    creditSpecification_cpuCredits,

    -- * CreditSpecificationRequest
    CreditSpecificationRequest (..),
    newCreditSpecificationRequest,
    creditSpecificationRequest_cpuCredits,

    -- * CustomerGateway
    CustomerGateway (..),
    newCustomerGateway,
    customerGateway_certificateArn,
    customerGateway_deviceName,
    customerGateway_tags,
    customerGateway_bgpAsn,
    customerGateway_customerGatewayId,
    customerGateway_ipAddress,
    customerGateway_state,
    customerGateway_type,

    -- * DataQuery
    DataQuery (..),
    newDataQuery,
    dataQuery_destination,
    dataQuery_id,
    dataQuery_metric,
    dataQuery_period,
    dataQuery_source,
    dataQuery_statistic,

    -- * DataResponse
    DataResponse (..),
    newDataResponse,
    dataResponse_destination,
    dataResponse_id,
    dataResponse_metric,
    dataResponse_metricPoints,
    dataResponse_period,
    dataResponse_source,
    dataResponse_statistic,

    -- * DeleteFleetError
    DeleteFleetError (..),
    newDeleteFleetError,
    deleteFleetError_code,
    deleteFleetError_message,

    -- * DeleteFleetErrorItem
    DeleteFleetErrorItem (..),
    newDeleteFleetErrorItem,
    deleteFleetErrorItem_error,
    deleteFleetErrorItem_fleetId,

    -- * DeleteFleetSuccessItem
    DeleteFleetSuccessItem (..),
    newDeleteFleetSuccessItem,
    deleteFleetSuccessItem_currentFleetState,
    deleteFleetSuccessItem_fleetId,
    deleteFleetSuccessItem_previousFleetState,

    -- * DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    newDeleteLaunchTemplateVersionsResponseErrorItem,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,

    -- * DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    newDeleteLaunchTemplateVersionsResponseSuccessItem,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,

    -- * DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (..),
    newDeleteQueuedReservedInstancesError,
    deleteQueuedReservedInstancesError_code,
    deleteQueuedReservedInstancesError_message,

    -- * DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    newDeregisterInstanceTagAttributeRequest,
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,

    -- * DescribeFastLaunchImagesSuccessItem
    DescribeFastLaunchImagesSuccessItem (..),
    newDescribeFastLaunchImagesSuccessItem,
    describeFastLaunchImagesSuccessItem_imageId,
    describeFastLaunchImagesSuccessItem_launchTemplate,
    describeFastLaunchImagesSuccessItem_maxParallelLaunches,
    describeFastLaunchImagesSuccessItem_ownerId,
    describeFastLaunchImagesSuccessItem_resourceType,
    describeFastLaunchImagesSuccessItem_snapshotConfiguration,
    describeFastLaunchImagesSuccessItem_state,
    describeFastLaunchImagesSuccessItem_stateTransitionReason,
    describeFastLaunchImagesSuccessItem_stateTransitionTime,

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    newDescribeFastSnapshotRestoreSuccessItem,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- * DescribeFleetError
    DescribeFleetError (..),
    newDescribeFleetError,
    describeFleetError_errorCode,
    describeFleetError_errorMessage,
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_lifecycle,

    -- * DescribeFleetsInstances
    DescribeFleetsInstances (..),
    newDescribeFleetsInstances,
    describeFleetsInstances_instanceIds,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_platform,

    -- * DestinationOptionsRequest
    DestinationOptionsRequest (..),
    newDestinationOptionsRequest,
    destinationOptionsRequest_fileFormat,
    destinationOptionsRequest_hiveCompatiblePartitions,
    destinationOptionsRequest_perHourPartition,

    -- * DestinationOptionsResponse
    DestinationOptionsResponse (..),
    newDestinationOptionsResponse,
    destinationOptionsResponse_fileFormat,
    destinationOptionsResponse_hiveCompatiblePartitions,
    destinationOptionsResponse_perHourPartition,

    -- * DeviceOptions
    DeviceOptions (..),
    newDeviceOptions,
    deviceOptions_tenantId,

    -- * DhcpConfiguration
    DhcpConfiguration (..),
    newDhcpConfiguration,
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- * DhcpOptions
    DhcpOptions (..),
    newDhcpOptions,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,
    dhcpOptions_ownerId,
    dhcpOptions_tags,

    -- * DirectoryServiceAuthentication
    DirectoryServiceAuthentication (..),
    newDirectoryServiceAuthentication,
    directoryServiceAuthentication_directoryId,

    -- * DirectoryServiceAuthenticationRequest
    DirectoryServiceAuthenticationRequest (..),
    newDirectoryServiceAuthenticationRequest,
    directoryServiceAuthenticationRequest_directoryId,

    -- * DisableFastSnapshotRestoreErrorItem
    DisableFastSnapshotRestoreErrorItem (..),
    newDisableFastSnapshotRestoreErrorItem,
    disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    disableFastSnapshotRestoreErrorItem_snapshotId,

    -- * DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (..),
    newDisableFastSnapshotRestoreStateError,
    disableFastSnapshotRestoreStateError_code,
    disableFastSnapshotRestoreStateError_message,

    -- * DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    newDisableFastSnapshotRestoreStateErrorItem,
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,
    disableFastSnapshotRestoreStateErrorItem_error,

    -- * DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    newDisableFastSnapshotRestoreSuccessItem,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- * DiskImage
    DiskImage (..),
    newDiskImage,
    diskImage_description,
    diskImage_image,
    diskImage_volume,

    -- * DiskImageDescription
    DiskImageDescription (..),
    newDiskImageDescription,
    diskImageDescription_checksum,
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,
    diskImageDescription_size,

    -- * DiskImageDetail
    DiskImageDetail (..),
    newDiskImageDetail,
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- * DiskImageVolumeDescription
    DiskImageVolumeDescription (..),
    newDiskImageVolumeDescription,
    diskImageVolumeDescription_id,
    diskImageVolumeDescription_size,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_count,
    diskInfo_sizeInGB,
    diskInfo_type,

    -- * DnsEntry
    DnsEntry (..),
    newDnsEntry,
    dnsEntry_dnsName,
    dnsEntry_hostedZoneId,

    -- * DnsOptions
    DnsOptions (..),
    newDnsOptions,
    dnsOptions_dnsRecordIpType,

    -- * DnsOptionsSpecification
    DnsOptionsSpecification (..),
    newDnsOptionsSpecification,
    dnsOptionsSpecification_dnsRecordIpType,

    -- * DnsServersOptionsModifyStructure
    DnsServersOptionsModifyStructure (..),
    newDnsServersOptionsModifyStructure,
    dnsServersOptionsModifyStructure_customDnsServers,
    dnsServersOptionsModifyStructure_enabled,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_iops,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_throughput,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_volumeType,

    -- * EbsInfo
    EbsInfo (..),
    newEbsInfo,
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,
    ebsInfo_nvmeSupport,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    newEbsInstanceBlockDevice,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_volumeId,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- * EbsOptimizedInfo
    EbsOptimizedInfo (..),
    newEbsOptimizedInfo,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_maximumThroughputInMBps,

    -- * EfaInfo
    EfaInfo (..),
    newEfaInfo,
    efaInfo_maximumEfaInterfaces,

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    newEgressOnlyInternetGateway,
    egressOnlyInternetGateway_attachments,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_tags,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    newElasticGpuAssociation,
    elasticGpuAssociation_elasticGpuAssociationId,
    elasticGpuAssociation_elasticGpuAssociationState,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuId,

    -- * ElasticGpuHealth
    ElasticGpuHealth (..),
    newElasticGpuHealth,
    elasticGpuHealth_status,

    -- * ElasticGpuSpecification
    ElasticGpuSpecification (..),
    newElasticGpuSpecification,
    elasticGpuSpecification_type,

    -- * ElasticGpuSpecificationResponse
    ElasticGpuSpecificationResponse (..),
    newElasticGpuSpecificationResponse,
    elasticGpuSpecificationResponse_type,

    -- * ElasticGpus
    ElasticGpus (..),
    newElasticGpus,
    elasticGpus_availabilityZone,
    elasticGpus_elasticGpuHealth,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuState,
    elasticGpus_elasticGpuType,
    elasticGpus_instanceId,
    elasticGpus_tags,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    newElasticInferenceAccelerator,
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    newElasticInferenceAcceleratorAssociation,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- * EnaSrdSpecification
    EnaSrdSpecification (..),
    newEnaSrdSpecification,
    enaSrdSpecification_enaSrdEnabled,
    enaSrdSpecification_enaSrdUdpSpecification,

    -- * EnaSrdUdpSpecification
    EnaSrdUdpSpecification (..),
    newEnaSrdUdpSpecification,
    enaSrdUdpSpecification_enaSrdUdpEnabled,

    -- * EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    newEnableFastSnapshotRestoreErrorItem,
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    enableFastSnapshotRestoreErrorItem_snapshotId,

    -- * EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (..),
    newEnableFastSnapshotRestoreStateError,
    enableFastSnapshotRestoreStateError_code,
    enableFastSnapshotRestoreStateError_message,

    -- * EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (..),
    newEnableFastSnapshotRestoreStateErrorItem,
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,
    enableFastSnapshotRestoreStateErrorItem_error,

    -- * EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    newEnableFastSnapshotRestoreSuccessItem,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- * EnclaveOptions
    EnclaveOptions (..),
    newEnclaveOptions,
    enclaveOptions_enabled,

    -- * EnclaveOptionsRequest
    EnclaveOptionsRequest (..),
    newEnclaveOptionsRequest,
    enclaveOptionsRequest_enabled,

    -- * EventInformation
    EventInformation (..),
    newEventInformation,
    eventInformation_eventDescription,
    eventInformation_eventSubType,
    eventInformation_instanceId,

    -- * Explanation
    Explanation (..),
    newExplanation,
    explanation_acl,
    explanation_aclRule,
    explanation_address,
    explanation_addresses,
    explanation_attachedTo,
    explanation_availabilityZones,
    explanation_cidrs,
    explanation_classicLoadBalancerListener,
    explanation_component,
    explanation_componentAccount,
    explanation_componentRegion,
    explanation_customerGateway,
    explanation_destination,
    explanation_destinationVpc,
    explanation_direction,
    explanation_elasticLoadBalancerListener,
    explanation_explanationCode,
    explanation_ingressRouteTable,
    explanation_internetGateway,
    explanation_loadBalancerArn,
    explanation_loadBalancerListenerPort,
    explanation_loadBalancerTarget,
    explanation_loadBalancerTargetGroup,
    explanation_loadBalancerTargetGroups,
    explanation_loadBalancerTargetPort,
    explanation_missingComponent,
    explanation_natGateway,
    explanation_networkInterface,
    explanation_packetField,
    explanation_port,
    explanation_portRanges,
    explanation_prefixList,
    explanation_protocols,
    explanation_routeTable,
    explanation_routeTableRoute,
    explanation_securityGroup,
    explanation_securityGroupRule,
    explanation_securityGroups,
    explanation_sourceVpc,
    explanation_state,
    explanation_subnet,
    explanation_subnetRouteTable,
    explanation_transitGateway,
    explanation_transitGatewayAttachment,
    explanation_transitGatewayRouteTable,
    explanation_transitGatewayRouteTableRoute,
    explanation_vpc,
    explanation_vpcEndpoint,
    explanation_vpcPeeringConnection,
    explanation_vpnConnection,
    explanation_vpnGateway,

    -- * ExportImageTask
    ExportImageTask (..),
    newExportImageTask,
    exportImageTask_description,
    exportImageTask_exportImageTaskId,
    exportImageTask_imageId,
    exportImageTask_progress,
    exportImageTask_s3ExportLocation,
    exportImageTask_status,
    exportImageTask_statusMessage,
    exportImageTask_tags,

    -- * ExportTask
    ExportTask (..),
    newExportTask,
    exportTask_tags,
    exportTask_description,
    exportTask_exportTaskId,
    exportTask_exportToS3Task,
    exportTask_instanceExportDetails,
    exportTask_state,
    exportTask_statusMessage,

    -- * ExportTaskS3Location
    ExportTaskS3Location (..),
    newExportTaskS3Location,
    exportTaskS3Location_s3Bucket,
    exportTaskS3Location_s3Prefix,

    -- * ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (..),
    newExportTaskS3LocationRequest,
    exportTaskS3LocationRequest_s3Prefix,
    exportTaskS3LocationRequest_s3Bucket,

    -- * ExportToS3Task
    ExportToS3Task (..),
    newExportToS3Task,
    exportToS3Task_containerFormat,
    exportToS3Task_diskImageFormat,
    exportToS3Task_s3Bucket,
    exportToS3Task_s3Key,

    -- * ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    newExportToS3TaskSpecification,
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_diskImageFormat,
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_s3Prefix,

    -- * FailedCapacityReservationFleetCancellationResult
    FailedCapacityReservationFleetCancellationResult (..),
    newFailedCapacityReservationFleetCancellationResult,
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    newFailedQueuedPurchaseDeletion,
    failedQueuedPurchaseDeletion_error,
    failedQueuedPurchaseDeletion_reservedInstancesId,

    -- * FastLaunchLaunchTemplateSpecificationRequest
    FastLaunchLaunchTemplateSpecificationRequest (..),
    newFastLaunchLaunchTemplateSpecificationRequest,
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName,
    fastLaunchLaunchTemplateSpecificationRequest_version,

    -- * FastLaunchLaunchTemplateSpecificationResponse
    FastLaunchLaunchTemplateSpecificationResponse (..),
    newFastLaunchLaunchTemplateSpecificationResponse,
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName,
    fastLaunchLaunchTemplateSpecificationResponse_version,

    -- * FastLaunchSnapshotConfigurationRequest
    FastLaunchSnapshotConfigurationRequest (..),
    newFastLaunchSnapshotConfigurationRequest,
    fastLaunchSnapshotConfigurationRequest_targetResourceCount,

    -- * FastLaunchSnapshotConfigurationResponse
    FastLaunchSnapshotConfigurationResponse (..),
    newFastLaunchSnapshotConfigurationResponse,
    fastLaunchSnapshotConfigurationResponse_targetResourceCount,

    -- * FederatedAuthentication
    FederatedAuthentication (..),
    newFederatedAuthentication,
    federatedAuthentication_samlProviderArn,
    federatedAuthentication_selfServiceSamlProviderArn,

    -- * FederatedAuthenticationRequest
    FederatedAuthenticationRequest (..),
    newFederatedAuthenticationRequest,
    federatedAuthenticationRequest_sAMLProviderArn,
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * FleetCapacityReservation
    FleetCapacityReservation (..),
    newFleetCapacityReservation,
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_availabilityZoneId,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_priority,
    fleetCapacityReservation_totalInstanceCount,
    fleetCapacityReservation_weight,

    -- * FleetData
    FleetData (..),
    newFleetData,
    fleetData_activityStatus,
    fleetData_clientToken,
    fleetData_context,
    fleetData_createTime,
    fleetData_errors,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_fleetId,
    fleetData_fleetState,
    fleetData_fulfilledCapacity,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_instances,
    fleetData_launchTemplateConfigs,
    fleetData_onDemandOptions,
    fleetData_replaceUnhealthyInstances,
    fleetData_spotOptions,
    fleetData_tags,
    fleetData_targetCapacitySpecification,
    fleetData_terminateInstancesWithExpiration,
    fleetData_type,
    fleetData_validFrom,
    fleetData_validUntil,

    -- * FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (..),
    newFleetLaunchTemplateConfig,
    fleetLaunchTemplateConfig_launchTemplateSpecification,
    fleetLaunchTemplateConfig_overrides,

    -- * FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (..),
    newFleetLaunchTemplateConfigRequest,
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,
    fleetLaunchTemplateConfigRequest_overrides,

    -- * FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (..),
    newFleetLaunchTemplateOverrides,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_imageId,
    fleetLaunchTemplateOverrides_instanceRequirements,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_weightedCapacity,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    newFleetLaunchTemplateOverridesRequest,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_imageId,
    fleetLaunchTemplateOverridesRequest_instanceRequirements,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_maxPrice,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,

    -- * FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    newFleetLaunchTemplateSpecification,
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_launchTemplateName,
    fleetLaunchTemplateSpecification_version,

    -- * FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    newFleetLaunchTemplateSpecificationRequest,
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,
    fleetLaunchTemplateSpecificationRequest_version,

    -- * FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (..),
    newFleetSpotCapacityRebalance,
    fleetSpotCapacityRebalance_replacementStrategy,
    fleetSpotCapacityRebalance_terminationDelay,

    -- * FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    newFleetSpotCapacityRebalanceRequest,
    fleetSpotCapacityRebalanceRequest_replacementStrategy,
    fleetSpotCapacityRebalanceRequest_terminationDelay,

    -- * FleetSpotMaintenanceStrategies
    FleetSpotMaintenanceStrategies (..),
    newFleetSpotMaintenanceStrategies,
    fleetSpotMaintenanceStrategies_capacityRebalance,

    -- * FleetSpotMaintenanceStrategiesRequest
    FleetSpotMaintenanceStrategiesRequest (..),
    newFleetSpotMaintenanceStrategiesRequest,
    fleetSpotMaintenanceStrategiesRequest_capacityRebalance,

    -- * FlowLog
    FlowLog (..),
    newFlowLog,
    flowLog_creationTime,
    flowLog_deliverCrossAccountRole,
    flowLog_deliverLogsErrorMessage,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsStatus,
    flowLog_destinationOptions,
    flowLog_flowLogId,
    flowLog_flowLogStatus,
    flowLog_logDestination,
    flowLog_logDestinationType,
    flowLog_logFormat,
    flowLog_logGroupName,
    flowLog_maxAggregationInterval,
    flowLog_resourceId,
    flowLog_tags,
    flowLog_trafficType,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    newFpgaDeviceInfo,
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_name,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    newFpgaDeviceMemoryInfo,
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- * FpgaImage
    FpgaImage (..),
    newFpgaImage,
    fpgaImage_createTime,
    fpgaImage_dataRetentionSupport,
    fpgaImage_description,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_fpgaImageId,
    fpgaImage_instanceTypes,
    fpgaImage_name,
    fpgaImage_ownerAlias,
    fpgaImage_ownerId,
    fpgaImage_pciId,
    fpgaImage_productCodes,
    fpgaImage_public,
    fpgaImage_shellVersion,
    fpgaImage_state,
    fpgaImage_tags,
    fpgaImage_updateTime,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    newFpgaImageAttribute,
    fpgaImageAttribute_description,
    fpgaImageAttribute_fpgaImageId,
    fpgaImageAttribute_loadPermissions,
    fpgaImageAttribute_name,
    fpgaImageAttribute_productCodes,

    -- * FpgaImageState
    FpgaImageState (..),
    newFpgaImageState,
    fpgaImageState_code,
    fpgaImageState_message,

    -- * FpgaInfo
    FpgaInfo (..),
    newFpgaInfo,
    fpgaInfo_fpgas,
    fpgaInfo_totalFpgaMemoryInMiB,

    -- * GpuDeviceInfo
    GpuDeviceInfo (..),
    newGpuDeviceInfo,
    gpuDeviceInfo_count,
    gpuDeviceInfo_manufacturer,
    gpuDeviceInfo_memoryInfo,
    gpuDeviceInfo_name,

    -- * GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (..),
    newGpuDeviceMemoryInfo,
    gpuDeviceMemoryInfo_sizeInMiB,

    -- * GpuInfo
    GpuInfo (..),
    newGpuInfo,
    gpuInfo_gpus,
    gpuInfo_totalGpuMemoryInMiB,

    -- * GroupIdentifier
    GroupIdentifier (..),
    newGroupIdentifier,
    groupIdentifier_groupId,
    groupIdentifier_groupName,

    -- * HibernationOptions
    HibernationOptions (..),
    newHibernationOptions,
    hibernationOptions_configured,

    -- * HibernationOptionsRequest
    HibernationOptionsRequest (..),
    newHibernationOptionsRequest,
    hibernationOptionsRequest_configured,

    -- * HistoryRecord
    HistoryRecord (..),
    newHistoryRecord,
    historyRecord_eventInformation,
    historyRecord_eventType,
    historyRecord_timestamp,

    -- * HistoryRecordEntry
    HistoryRecordEntry (..),
    newHistoryRecordEntry,
    historyRecordEntry_eventInformation,
    historyRecordEntry_eventType,
    historyRecordEntry_timestamp,

    -- * Host
    Host (..),
    newHost,
    host_allocationTime,
    host_allowsMultipleInstanceTypes,
    host_autoPlacement,
    host_availabilityZone,
    host_availabilityZoneId,
    host_availableCapacity,
    host_clientToken,
    host_hostId,
    host_hostProperties,
    host_hostRecovery,
    host_hostReservationId,
    host_instances,
    host_memberOfServiceLinkedResourceGroup,
    host_outpostArn,
    host_ownerId,
    host_releaseTime,
    host_state,
    host_tags,

    -- * HostInstance
    HostInstance (..),
    newHostInstance,
    hostInstance_instanceId,
    hostInstance_instanceType,
    hostInstance_ownerId,

    -- * HostOffering
    HostOffering (..),
    newHostOffering,
    hostOffering_currencyCode,
    hostOffering_duration,
    hostOffering_hourlyPrice,
    hostOffering_instanceFamily,
    hostOffering_offeringId,
    hostOffering_paymentOption,
    hostOffering_upfrontPrice,

    -- * HostProperties
    HostProperties (..),
    newHostProperties,
    hostProperties_cores,
    hostProperties_instanceFamily,
    hostProperties_instanceType,
    hostProperties_sockets,
    hostProperties_totalVCpus,

    -- * HostReservation
    HostReservation (..),
    newHostReservation,
    hostReservation_count,
    hostReservation_currencyCode,
    hostReservation_duration,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_hostReservationId,
    hostReservation_hourlyPrice,
    hostReservation_instanceFamily,
    hostReservation_offeringId,
    hostReservation_paymentOption,
    hostReservation_start,
    hostReservation_state,
    hostReservation_tags,
    hostReservation_upfrontPrice,

    -- * IKEVersionsListValue
    IKEVersionsListValue (..),
    newIKEVersionsListValue,
    iKEVersionsListValue_value,

    -- * IKEVersionsRequestListValue
    IKEVersionsRequestListValue (..),
    newIKEVersionsRequestListValue,
    iKEVersionsRequestListValue_value,

    -- * IamInstanceProfile
    IamInstanceProfile (..),
    newIamInstanceProfile,
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- * IamInstanceProfileAssociation
    IamInstanceProfileAssociation (..),
    newIamInstanceProfileAssociation,
    iamInstanceProfileAssociation_associationId,
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_timestamp,

    -- * IamInstanceProfileSpecification
    IamInstanceProfileSpecification (..),
    newIamInstanceProfileSpecification,
    iamInstanceProfileSpecification_arn,
    iamInstanceProfileSpecification_name,

    -- * IcmpTypeCode
    IcmpTypeCode (..),
    newIcmpTypeCode,
    icmpTypeCode_code,
    icmpTypeCode_type,

    -- * IdFormat
    IdFormat (..),
    newIdFormat,
    idFormat_deadline,
    idFormat_resource,
    idFormat_useLongIds,

    -- * Image
    Image (..),
    newImage,
    image_blockDeviceMappings,
    image_bootMode,
    image_creationDate,
    image_deprecationTime,
    image_description,
    image_enaSupport,
    image_imageOwnerAlias,
    image_imdsSupport,
    image_kernelId,
    image_name,
    image_platform,
    image_platformDetails,
    image_productCodes,
    image_ramdiskId,
    image_rootDeviceName,
    image_sriovNetSupport,
    image_stateReason,
    image_tags,
    image_tpmSupport,
    image_usageOperation,
    image_imageId,
    image_imageLocation,
    image_state,
    image_ownerId,
    image_public,
    image_architecture,
    image_imageType,
    image_rootDeviceType,
    image_virtualizationType,
    image_hypervisor,

    -- * ImageDiskContainer
    ImageDiskContainer (..),
    newImageDiskContainer,
    imageDiskContainer_description,
    imageDiskContainer_deviceName,
    imageDiskContainer_format,
    imageDiskContainer_snapshotId,
    imageDiskContainer_url,
    imageDiskContainer_userBucket,

    -- * ImageRecycleBinInfo
    ImageRecycleBinInfo (..),
    newImageRecycleBinInfo,
    imageRecycleBinInfo_description,
    imageRecycleBinInfo_imageId,
    imageRecycleBinInfo_name,
    imageRecycleBinInfo_recycleBinEnterTime,
    imageRecycleBinInfo_recycleBinExitTime,

    -- * ImportImageLicenseConfigurationRequest
    ImportImageLicenseConfigurationRequest (..),
    newImportImageLicenseConfigurationRequest,
    importImageLicenseConfigurationRequest_licenseConfigurationArn,

    -- * ImportImageLicenseConfigurationResponse
    ImportImageLicenseConfigurationResponse (..),
    newImportImageLicenseConfigurationResponse,
    importImageLicenseConfigurationResponse_licenseConfigurationArn,

    -- * ImportImageTask
    ImportImageTask (..),
    newImportImageTask,
    importImageTask_architecture,
    importImageTask_bootMode,
    importImageTask_description,
    importImageTask_encrypted,
    importImageTask_hypervisor,
    importImageTask_imageId,
    importImageTask_importTaskId,
    importImageTask_kmsKeyId,
    importImageTask_licenseSpecifications,
    importImageTask_licenseType,
    importImageTask_platform,
    importImageTask_progress,
    importImageTask_snapshotDetails,
    importImageTask_status,
    importImageTask_statusMessage,
    importImageTask_tags,
    importImageTask_usageOperation,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    newImportInstanceLaunchSpecification,
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_userData,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    newImportInstanceTaskDetails,
    importInstanceTaskDetails_description,
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_volumes,

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    newImportInstanceVolumeDetailItem,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_description,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_volume,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    newImportSnapshotTask,
    importSnapshotTask_description,
    importSnapshotTask_importTaskId,
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_tags,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    newImportVolumeTaskDetails,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_description,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_volume,

    -- * InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    newInferenceAcceleratorInfo,
    inferenceAcceleratorInfo_accelerators,

    -- * InferenceDeviceInfo
    InferenceDeviceInfo (..),
    newInferenceDeviceInfo,
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_manufacturer,
    inferenceDeviceInfo_name,

    -- * Instance
    Instance (..),
    newInstance,
    instance_blockDeviceMappings,
    instance_bootMode,
    instance_capacityReservationId,
    instance_capacityReservationSpecification,
    instance_clientToken,
    instance_cpuOptions,
    instance_ebsOptimized,
    instance_elasticGpuAssociations,
    instance_elasticInferenceAcceleratorAssociations,
    instance_enaSupport,
    instance_enclaveOptions,
    instance_hibernationOptions,
    instance_iamInstanceProfile,
    instance_instanceLifecycle,
    instance_ipv6Address,
    instance_kernelId,
    instance_keyName,
    instance_licenses,
    instance_maintenanceOptions,
    instance_metadataOptions,
    instance_networkInterfaces,
    instance_outpostArn,
    instance_platform,
    instance_platformDetails,
    instance_privateDnsName,
    instance_privateDnsNameOptions,
    instance_privateIpAddress,
    instance_productCodes,
    instance_publicDnsName,
    instance_publicIpAddress,
    instance_ramdiskId,
    instance_rootDeviceName,
    instance_securityGroups,
    instance_sourceDestCheck,
    instance_spotInstanceRequestId,
    instance_sriovNetSupport,
    instance_stateReason,
    instance_stateTransitionReason,
    instance_subnetId,
    instance_tags,
    instance_tpmSupport,
    instance_usageOperation,
    instance_usageOperationUpdateTime,
    instance_vpcId,
    instance_instanceId,
    instance_imageId,
    instance_amiLaunchIndex,
    instance_instanceType,
    instance_launchTime,
    instance_placement,
    instance_monitoring,
    instance_architecture,
    instance_rootDeviceType,
    instance_virtualizationType,
    instance_hypervisor,
    instance_state,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    newInstanceBlockDeviceMapping,
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_ebs,

    -- * InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (..),
    newInstanceBlockDeviceMappingSpecification,
    instanceBlockDeviceMappingSpecification_deviceName,
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    newInstanceCapacity,
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,
    instanceCapacity_totalCapacity,

    -- * InstanceCount
    InstanceCount (..),
    newInstanceCount,
    instanceCount_instanceCount,
    instanceCount_state,

    -- * InstanceCreditSpecification
    InstanceCreditSpecification (..),
    newInstanceCreditSpecification,
    instanceCreditSpecification_cpuCredits,
    instanceCreditSpecification_instanceId,

    -- * InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (..),
    newInstanceCreditSpecificationRequest,
    instanceCreditSpecificationRequest_cpuCredits,
    instanceCreditSpecificationRequest_instanceId,

    -- * InstanceEventWindow
    InstanceEventWindow (..),
    newInstanceEventWindow,
    instanceEventWindow_associationTarget,
    instanceEventWindow_cronExpression,
    instanceEventWindow_instanceEventWindowId,
    instanceEventWindow_name,
    instanceEventWindow_state,
    instanceEventWindow_tags,
    instanceEventWindow_timeRanges,

    -- * InstanceEventWindowAssociationRequest
    InstanceEventWindowAssociationRequest (..),
    newInstanceEventWindowAssociationRequest,
    instanceEventWindowAssociationRequest_dedicatedHostIds,
    instanceEventWindowAssociationRequest_instanceIds,
    instanceEventWindowAssociationRequest_instanceTags,

    -- * InstanceEventWindowAssociationTarget
    InstanceEventWindowAssociationTarget (..),
    newInstanceEventWindowAssociationTarget,
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_instanceIds,
    instanceEventWindowAssociationTarget_tags,

    -- * InstanceEventWindowDisassociationRequest
    InstanceEventWindowDisassociationRequest (..),
    newInstanceEventWindowDisassociationRequest,
    instanceEventWindowDisassociationRequest_dedicatedHostIds,
    instanceEventWindowDisassociationRequest_instanceIds,
    instanceEventWindowDisassociationRequest_instanceTags,

    -- * InstanceEventWindowStateChange
    InstanceEventWindowStateChange (..),
    newInstanceEventWindowStateChange,
    instanceEventWindowStateChange_instanceEventWindowId,
    instanceEventWindowStateChange_state,

    -- * InstanceEventWindowTimeRange
    InstanceEventWindowTimeRange (..),
    newInstanceEventWindowTimeRange,
    instanceEventWindowTimeRange_endHour,
    instanceEventWindowTimeRange_endWeekDay,
    instanceEventWindowTimeRange_startHour,
    instanceEventWindowTimeRange_startWeekDay,

    -- * InstanceEventWindowTimeRangeRequest
    InstanceEventWindowTimeRangeRequest (..),
    newInstanceEventWindowTimeRangeRequest,
    instanceEventWindowTimeRangeRequest_endHour,
    instanceEventWindowTimeRangeRequest_endWeekDay,
    instanceEventWindowTimeRangeRequest_startHour,
    instanceEventWindowTimeRangeRequest_startWeekDay,

    -- * InstanceExportDetails
    InstanceExportDetails (..),
    newInstanceExportDetails,
    instanceExportDetails_instanceId,
    instanceExportDetails_targetEnvironment,

    -- * InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    newInstanceFamilyCreditSpecification,
    instanceFamilyCreditSpecification_cpuCredits,
    instanceFamilyCreditSpecification_instanceFamily,

    -- * InstanceIpv4Prefix
    InstanceIpv4Prefix (..),
    newInstanceIpv4Prefix,
    instanceIpv4Prefix_ipv4Prefix,

    -- * InstanceIpv6Address
    InstanceIpv6Address (..),
    newInstanceIpv6Address,
    instanceIpv6Address_ipv6Address,

    -- * InstanceIpv6AddressRequest
    InstanceIpv6AddressRequest (..),
    newInstanceIpv6AddressRequest,
    instanceIpv6AddressRequest_ipv6Address,

    -- * InstanceIpv6Prefix
    InstanceIpv6Prefix (..),
    newInstanceIpv6Prefix,
    instanceIpv6Prefix_ipv6Prefix,

    -- * InstanceMaintenanceOptions
    InstanceMaintenanceOptions (..),
    newInstanceMaintenanceOptions,
    instanceMaintenanceOptions_autoRecovery,

    -- * InstanceMaintenanceOptionsRequest
    InstanceMaintenanceOptionsRequest (..),
    newInstanceMaintenanceOptionsRequest,
    instanceMaintenanceOptionsRequest_autoRecovery,

    -- * InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    newInstanceMarketOptionsRequest,
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- * InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    newInstanceMetadataOptionsRequest,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpProtocolIpv6,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,
    instanceMetadataOptionsRequest_instanceMetadataTags,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    newInstanceMetadataOptionsResponse,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpProtocolIpv6,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_httpTokens,
    instanceMetadataOptionsResponse_instanceMetadataTags,
    instanceMetadataOptionsResponse_state,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    newInstanceMonitoring,
    instanceMonitoring_instanceId,
    instanceMonitoring_monitoring,

    -- * InstanceNetworkInterface
    InstanceNetworkInterface (..),
    newInstanceNetworkInterface,
    instanceNetworkInterface_association,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_description,
    instanceNetworkInterface_groups,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_ipv6Addresses,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_status,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_vpcId,

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    newInstanceNetworkInterfaceAssociation,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_customerOwnedIp,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_publicIp,

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    newInstanceNetworkInterfaceAttachment,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_deviceIndex,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_status,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    newInstanceNetworkInterfaceSpecification,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_subnetId,

    -- * InstancePrivateIpAddress
    InstancePrivateIpAddress (..),
    newInstancePrivateIpAddress,
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_privateIpAddress,

    -- * InstanceRequirements
    InstanceRequirements (..),
    newInstanceRequirements,
    instanceRequirements_acceleratorCount,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_acceleratorNames,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_bareMetal,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_instanceGenerations,
    instanceRequirements_localStorage,
    instanceRequirements_localStorageTypes,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_memoryMiB,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_vCpuCount,

    -- * InstanceRequirementsRequest
    InstanceRequirementsRequest (..),
    newInstanceRequirementsRequest,
    instanceRequirementsRequest_acceleratorCount,
    instanceRequirementsRequest_acceleratorManufacturers,
    instanceRequirementsRequest_acceleratorNames,
    instanceRequirementsRequest_acceleratorTotalMemoryMiB,
    instanceRequirementsRequest_acceleratorTypes,
    instanceRequirementsRequest_allowedInstanceTypes,
    instanceRequirementsRequest_bareMetal,
    instanceRequirementsRequest_baselineEbsBandwidthMbps,
    instanceRequirementsRequest_burstablePerformance,
    instanceRequirementsRequest_cpuManufacturers,
    instanceRequirementsRequest_excludedInstanceTypes,
    instanceRequirementsRequest_instanceGenerations,
    instanceRequirementsRequest_localStorage,
    instanceRequirementsRequest_localStorageTypes,
    instanceRequirementsRequest_memoryGiBPerVCpu,
    instanceRequirementsRequest_networkBandwidthGbps,
    instanceRequirementsRequest_networkInterfaceCount,
    instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_requireHibernateSupport,
    instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_totalLocalStorageGB,
    instanceRequirementsRequest_vCpuCount,
    instanceRequirementsRequest_memoryMiB,

    -- * InstanceRequirementsWithMetadataRequest
    InstanceRequirementsWithMetadataRequest (..),
    newInstanceRequirementsWithMetadataRequest,
    instanceRequirementsWithMetadataRequest_architectureTypes,
    instanceRequirementsWithMetadataRequest_instanceRequirements,
    instanceRequirementsWithMetadataRequest_virtualizationTypes,

    -- * InstanceSpecification
    InstanceSpecification (..),
    newInstanceSpecification,
    instanceSpecification_excludeBootVolume,
    instanceSpecification_excludeDataVolumeIds,
    instanceSpecification_instanceId,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_name,
    instanceState_code,

    -- * InstanceStateChange
    InstanceStateChange (..),
    newInstanceStateChange,
    instanceStateChange_currentState,
    instanceStateChange_instanceId,
    instanceStateChange_previousState,

    -- * InstanceStatus
    InstanceStatus (..),
    newInstanceStatus,
    instanceStatus_availabilityZone,
    instanceStatus_events,
    instanceStatus_instanceId,
    instanceStatus_instanceState,
    instanceStatus_instanceStatus,
    instanceStatus_outpostArn,
    instanceStatus_systemStatus,

    -- * InstanceStatusDetails
    InstanceStatusDetails (..),
    newInstanceStatusDetails,
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,
    instanceStatusDetails_status,

    -- * InstanceStatusEvent
    InstanceStatusEvent (..),
    newInstanceStatusEvent,
    instanceStatusEvent_code,
    instanceStatusEvent_description,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_notAfter,
    instanceStatusEvent_notBefore,
    instanceStatusEvent_notBeforeDeadline,

    -- * InstanceStatusSummary
    InstanceStatusSummary (..),
    newInstanceStatusSummary,
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- * InstanceStorageInfo
    InstanceStorageInfo (..),
    newInstanceStorageInfo,
    instanceStorageInfo_disks,
    instanceStorageInfo_encryptionSupport,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_totalSizeInGB,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    newInstanceTagNotificationAttribute,
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    newInstanceTypeInfo,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_supportedBootModes,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_vCpuInfo,

    -- * InstanceTypeInfoFromInstanceRequirements
    InstanceTypeInfoFromInstanceRequirements (..),
    newInstanceTypeInfoFromInstanceRequirements,
    instanceTypeInfoFromInstanceRequirements_instanceType,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    newInstanceTypeOffering,
    instanceTypeOffering_instanceType,
    instanceTypeOffering_location,
    instanceTypeOffering_locationType,

    -- * InstanceUsage
    InstanceUsage (..),
    newInstanceUsage,
    instanceUsage_accountId,
    instanceUsage_usedInstanceCount,

    -- * IntegrateServices
    IntegrateServices (..),
    newIntegrateServices,
    integrateServices_athenaIntegrations,

    -- * InternetGateway
    InternetGateway (..),
    newInternetGateway,
    internetGateway_attachments,
    internetGateway_ownerId,
    internetGateway_tags,
    internetGateway_internetGatewayId,

    -- * InternetGatewayAttachment
    InternetGatewayAttachment (..),
    newInternetGatewayAttachment,
    internetGatewayAttachment_state,
    internetGatewayAttachment_vpcId,

    -- * IpPermission
    IpPermission (..),
    newIpPermission,
    ipPermission_fromPort,
    ipPermission_ipRanges,
    ipPermission_ipv6Ranges,
    ipPermission_prefixListIds,
    ipPermission_toPort,
    ipPermission_userIdGroupPairs,
    ipPermission_ipProtocol,

    -- * IpRange
    IpRange (..),
    newIpRange,
    ipRange_description,
    ipRange_cidrIp,

    -- * Ipam
    Ipam (..),
    newIpam,
    ipam_description,
    ipam_ipamArn,
    ipam_ipamId,
    ipam_ipamRegion,
    ipam_operatingRegions,
    ipam_ownerId,
    ipam_privateDefaultScopeId,
    ipam_publicDefaultScopeId,
    ipam_scopeCount,
    ipam_state,
    ipam_tags,

    -- * IpamAddressHistoryRecord
    IpamAddressHistoryRecord (..),
    newIpamAddressHistoryRecord,
    ipamAddressHistoryRecord_resourceCidr,
    ipamAddressHistoryRecord_resourceComplianceStatus,
    ipamAddressHistoryRecord_resourceId,
    ipamAddressHistoryRecord_resourceName,
    ipamAddressHistoryRecord_resourceOverlapStatus,
    ipamAddressHistoryRecord_resourceOwnerId,
    ipamAddressHistoryRecord_resourceRegion,
    ipamAddressHistoryRecord_resourceType,
    ipamAddressHistoryRecord_sampledEndTime,
    ipamAddressHistoryRecord_sampledStartTime,
    ipamAddressHistoryRecord_vpcId,

    -- * IpamCidrAuthorizationContext
    IpamCidrAuthorizationContext (..),
    newIpamCidrAuthorizationContext,
    ipamCidrAuthorizationContext_message,
    ipamCidrAuthorizationContext_signature,

    -- * IpamOperatingRegion
    IpamOperatingRegion (..),
    newIpamOperatingRegion,
    ipamOperatingRegion_regionName,

    -- * IpamPool
    IpamPool (..),
    newIpamPool,
    ipamPool_addressFamily,
    ipamPool_allocationDefaultNetmaskLength,
    ipamPool_allocationMaxNetmaskLength,
    ipamPool_allocationMinNetmaskLength,
    ipamPool_allocationResourceTags,
    ipamPool_autoImport,
    ipamPool_awsService,
    ipamPool_description,
    ipamPool_ipamArn,
    ipamPool_ipamPoolArn,
    ipamPool_ipamPoolId,
    ipamPool_ipamRegion,
    ipamPool_ipamScopeArn,
    ipamPool_ipamScopeType,
    ipamPool_locale,
    ipamPool_ownerId,
    ipamPool_poolDepth,
    ipamPool_publiclyAdvertisable,
    ipamPool_sourceIpamPoolId,
    ipamPool_state,
    ipamPool_stateMessage,
    ipamPool_tags,

    -- * IpamPoolAllocation
    IpamPoolAllocation (..),
    newIpamPoolAllocation,
    ipamPoolAllocation_cidr,
    ipamPoolAllocation_description,
    ipamPoolAllocation_ipamPoolAllocationId,
    ipamPoolAllocation_resourceId,
    ipamPoolAllocation_resourceOwner,
    ipamPoolAllocation_resourceRegion,
    ipamPoolAllocation_resourceType,

    -- * IpamPoolCidr
    IpamPoolCidr (..),
    newIpamPoolCidr,
    ipamPoolCidr_cidr,
    ipamPoolCidr_failureReason,
    ipamPoolCidr_state,

    -- * IpamPoolCidrFailureReason
    IpamPoolCidrFailureReason (..),
    newIpamPoolCidrFailureReason,
    ipamPoolCidrFailureReason_code,
    ipamPoolCidrFailureReason_message,

    -- * IpamResourceCidr
    IpamResourceCidr (..),
    newIpamResourceCidr,
    ipamResourceCidr_complianceStatus,
    ipamResourceCidr_ipUsage,
    ipamResourceCidr_ipamId,
    ipamResourceCidr_ipamPoolId,
    ipamResourceCidr_ipamScopeId,
    ipamResourceCidr_managementState,
    ipamResourceCidr_overlapStatus,
    ipamResourceCidr_resourceCidr,
    ipamResourceCidr_resourceId,
    ipamResourceCidr_resourceName,
    ipamResourceCidr_resourceOwnerId,
    ipamResourceCidr_resourceRegion,
    ipamResourceCidr_resourceTags,
    ipamResourceCidr_resourceType,
    ipamResourceCidr_vpcId,

    -- * IpamResourceTag
    IpamResourceTag (..),
    newIpamResourceTag,
    ipamResourceTag_key,
    ipamResourceTag_value,

    -- * IpamScope
    IpamScope (..),
    newIpamScope,
    ipamScope_description,
    ipamScope_ipamArn,
    ipamScope_ipamRegion,
    ipamScope_ipamScopeArn,
    ipamScope_ipamScopeId,
    ipamScope_ipamScopeType,
    ipamScope_isDefault,
    ipamScope_ownerId,
    ipamScope_poolCount,
    ipamScope_state,
    ipamScope_tags,

    -- * Ipv4PrefixSpecification
    Ipv4PrefixSpecification (..),
    newIpv4PrefixSpecification,
    ipv4PrefixSpecification_ipv4Prefix,

    -- * Ipv4PrefixSpecificationRequest
    Ipv4PrefixSpecificationRequest (..),
    newIpv4PrefixSpecificationRequest,
    ipv4PrefixSpecificationRequest_ipv4Prefix,

    -- * Ipv4PrefixSpecificationResponse
    Ipv4PrefixSpecificationResponse (..),
    newIpv4PrefixSpecificationResponse,
    ipv4PrefixSpecificationResponse_ipv4Prefix,

    -- * Ipv6CidrAssociation
    Ipv6CidrAssociation (..),
    newIpv6CidrAssociation,
    ipv6CidrAssociation_associatedResource,
    ipv6CidrAssociation_ipv6Cidr,

    -- * Ipv6CidrBlock
    Ipv6CidrBlock (..),
    newIpv6CidrBlock,
    ipv6CidrBlock_ipv6CidrBlock,

    -- * Ipv6Pool
    Ipv6Pool (..),
    newIpv6Pool,
    ipv6Pool_description,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_poolId,
    ipv6Pool_tags,

    -- * Ipv6PrefixSpecification
    Ipv6PrefixSpecification (..),
    newIpv6PrefixSpecification,
    ipv6PrefixSpecification_ipv6Prefix,

    -- * Ipv6PrefixSpecificationRequest
    Ipv6PrefixSpecificationRequest (..),
    newIpv6PrefixSpecificationRequest,
    ipv6PrefixSpecificationRequest_ipv6Prefix,

    -- * Ipv6PrefixSpecificationResponse
    Ipv6PrefixSpecificationResponse (..),
    newIpv6PrefixSpecificationResponse,
    ipv6PrefixSpecificationResponse_ipv6Prefix,

    -- * Ipv6Range
    Ipv6Range (..),
    newIpv6Range,
    ipv6Range_cidrIpv6,
    ipv6Range_description,

    -- * KeyPairInfo
    KeyPairInfo (..),
    newKeyPairInfo,
    keyPairInfo_createTime,
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyName,
    keyPairInfo_keyPairId,
    keyPairInfo_keyType,
    keyPairInfo_publicKey,
    keyPairInfo_tags,

    -- * LastError
    LastError (..),
    newLastError,
    lastError_code,
    lastError_message,

    -- * LaunchPermission
    LaunchPermission (..),
    newLaunchPermission,
    launchPermission_group,
    launchPermission_organizationArn,
    launchPermission_organizationalUnitArn,
    launchPermission_userId,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    newLaunchPermissionModifications,
    launchPermissionModifications_add,
    launchPermissionModifications_remove,

    -- * LaunchSpecification
    LaunchSpecification (..),
    newLaunchSpecification,
    launchSpecification_addressingType,
    launchSpecification_blockDeviceMappings,
    launchSpecification_ebsOptimized,
    launchSpecification_iamInstanceProfile,
    launchSpecification_imageId,
    launchSpecification_instanceType,
    launchSpecification_kernelId,
    launchSpecification_keyName,
    launchSpecification_monitoring,
    launchSpecification_networkInterfaces,
    launchSpecification_placement,
    launchSpecification_ramdiskId,
    launchSpecification_securityGroups,
    launchSpecification_subnetId,
    launchSpecification_userData,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_launchTemplateName,
    launchTemplate_tags,

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    newLaunchTemplateAndOverridesResponse,
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    newLaunchTemplateBlockDeviceMapping,
    launchTemplateBlockDeviceMapping_deviceName,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    newLaunchTemplateBlockDeviceMappingRequest,
    launchTemplateBlockDeviceMappingRequest_deviceName,
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_virtualName,

    -- * LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (..),
    newLaunchTemplateCapacityReservationSpecificationRequest,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget,

    -- * LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (..),
    newLaunchTemplateCapacityReservationSpecificationResponse,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget,

    -- * LaunchTemplateConfig
    LaunchTemplateConfig (..),
    newLaunchTemplateConfig,
    launchTemplateConfig_launchTemplateSpecification,
    launchTemplateConfig_overrides,

    -- * LaunchTemplateCpuOptions
    LaunchTemplateCpuOptions (..),
    newLaunchTemplateCpuOptions,
    launchTemplateCpuOptions_coreCount,
    launchTemplateCpuOptions_threadsPerCore,

    -- * LaunchTemplateCpuOptionsRequest
    LaunchTemplateCpuOptionsRequest (..),
    newLaunchTemplateCpuOptionsRequest,
    launchTemplateCpuOptionsRequest_coreCount,
    launchTemplateCpuOptionsRequest_threadsPerCore,

    -- * LaunchTemplateEbsBlockDevice
    LaunchTemplateEbsBlockDevice (..),
    newLaunchTemplateEbsBlockDevice,
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_iops,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_volumeType,

    -- * LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (..),
    newLaunchTemplateEbsBlockDeviceRequest,
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_iops,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_volumeType,

    -- * LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (..),
    newLaunchTemplateElasticInferenceAccelerator,
    launchTemplateElasticInferenceAccelerator_count,
    launchTemplateElasticInferenceAccelerator_type,

    -- * LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (..),
    newLaunchTemplateElasticInferenceAcceleratorResponse,
    launchTemplateElasticInferenceAcceleratorResponse_count,
    launchTemplateElasticInferenceAcceleratorResponse_type,

    -- * LaunchTemplateEnclaveOptions
    LaunchTemplateEnclaveOptions (..),
    newLaunchTemplateEnclaveOptions,
    launchTemplateEnclaveOptions_enabled,

    -- * LaunchTemplateEnclaveOptionsRequest
    LaunchTemplateEnclaveOptionsRequest (..),
    newLaunchTemplateEnclaveOptionsRequest,
    launchTemplateEnclaveOptionsRequest_enabled,

    -- * LaunchTemplateHibernationOptions
    LaunchTemplateHibernationOptions (..),
    newLaunchTemplateHibernationOptions,
    launchTemplateHibernationOptions_configured,

    -- * LaunchTemplateHibernationOptionsRequest
    LaunchTemplateHibernationOptionsRequest (..),
    newLaunchTemplateHibernationOptionsRequest,
    launchTemplateHibernationOptionsRequest_configured,

    -- * LaunchTemplateIamInstanceProfileSpecification
    LaunchTemplateIamInstanceProfileSpecification (..),
    newLaunchTemplateIamInstanceProfileSpecification,
    launchTemplateIamInstanceProfileSpecification_arn,
    launchTemplateIamInstanceProfileSpecification_name,

    -- * LaunchTemplateIamInstanceProfileSpecificationRequest
    LaunchTemplateIamInstanceProfileSpecificationRequest (..),
    newLaunchTemplateIamInstanceProfileSpecificationRequest,
    launchTemplateIamInstanceProfileSpecificationRequest_arn,
    launchTemplateIamInstanceProfileSpecificationRequest_name,

    -- * LaunchTemplateInstanceMaintenanceOptions
    LaunchTemplateInstanceMaintenanceOptions (..),
    newLaunchTemplateInstanceMaintenanceOptions,
    launchTemplateInstanceMaintenanceOptions_autoRecovery,

    -- * LaunchTemplateInstanceMaintenanceOptionsRequest
    LaunchTemplateInstanceMaintenanceOptionsRequest (..),
    newLaunchTemplateInstanceMaintenanceOptionsRequest,
    launchTemplateInstanceMaintenanceOptionsRequest_autoRecovery,

    -- * LaunchTemplateInstanceMarketOptions
    LaunchTemplateInstanceMarketOptions (..),
    newLaunchTemplateInstanceMarketOptions,
    launchTemplateInstanceMarketOptions_marketType,
    launchTemplateInstanceMarketOptions_spotOptions,

    -- * LaunchTemplateInstanceMarketOptionsRequest
    LaunchTemplateInstanceMarketOptionsRequest (..),
    newLaunchTemplateInstanceMarketOptionsRequest,
    launchTemplateInstanceMarketOptionsRequest_marketType,
    launchTemplateInstanceMarketOptionsRequest_spotOptions,

    -- * LaunchTemplateInstanceMetadataOptions
    LaunchTemplateInstanceMetadataOptions (..),
    newLaunchTemplateInstanceMetadataOptions,
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_httpTokens,
    launchTemplateInstanceMetadataOptions_instanceMetadataTags,
    launchTemplateInstanceMetadataOptions_state,

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    newLaunchTemplateInstanceMetadataOptionsRequest,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,
    launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecification,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,

    -- * LaunchTemplateLicenseConfiguration
    LaunchTemplateLicenseConfiguration (..),
    newLaunchTemplateLicenseConfiguration,
    launchTemplateLicenseConfiguration_licenseConfigurationArn,

    -- * LaunchTemplateLicenseConfigurationRequest
    LaunchTemplateLicenseConfigurationRequest (..),
    newLaunchTemplateLicenseConfigurationRequest,
    launchTemplateLicenseConfigurationRequest_licenseConfigurationArn,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    newLaunchTemplateOverrides,
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_weightedCapacity,

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    newLaunchTemplatePlacement,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_groupId,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_tenancy,

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    newLaunchTemplatePlacementRequest,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_groupId,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_tenancy,

    -- * LaunchTemplatePrivateDnsNameOptions
    LaunchTemplatePrivateDnsNameOptions (..),
    newLaunchTemplatePrivateDnsNameOptions,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptions_hostnameType,

    -- * LaunchTemplatePrivateDnsNameOptionsRequest
    LaunchTemplatePrivateDnsNameOptionsRequest (..),
    newLaunchTemplatePrivateDnsNameOptionsRequest,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptionsRequest_hostnameType,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- * LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (..),
    newLaunchTemplateSpotMarketOptions,
    launchTemplateSpotMarketOptions_blockDurationMinutes,
    launchTemplateSpotMarketOptions_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptions_maxPrice,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_validUntil,

    -- * LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    newLaunchTemplateSpotMarketOptionsRequest,
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_maxPrice,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_validUntil,

    -- * LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (..),
    newLaunchTemplateTagSpecification,
    launchTemplateTagSpecification_resourceType,
    launchTemplateTagSpecification_tags,

    -- * LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (..),
    newLaunchTemplateTagSpecificationRequest,
    launchTemplateTagSpecificationRequest_resourceType,
    launchTemplateTagSpecificationRequest_tags,

    -- * LaunchTemplateVersion
    LaunchTemplateVersion (..),
    newLaunchTemplateVersion,
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,

    -- * LaunchTemplatesMonitoring
    LaunchTemplatesMonitoring (..),
    newLaunchTemplatesMonitoring,
    launchTemplatesMonitoring_enabled,

    -- * LaunchTemplatesMonitoringRequest
    LaunchTemplatesMonitoringRequest (..),
    newLaunchTemplatesMonitoringRequest,
    launchTemplatesMonitoringRequest_enabled,

    -- * LicenseConfiguration
    LicenseConfiguration (..),
    newLicenseConfiguration,
    licenseConfiguration_licenseConfigurationArn,

    -- * LicenseConfigurationRequest
    LicenseConfigurationRequest (..),
    newLicenseConfigurationRequest,
    licenseConfigurationRequest_licenseConfigurationArn,

    -- * LoadBalancersConfig
    LoadBalancersConfig (..),
    newLoadBalancersConfig,
    loadBalancersConfig_classicLoadBalancersConfig,
    loadBalancersConfig_targetGroupsConfig,

    -- * LoadPermission
    LoadPermission (..),
    newLoadPermission,
    loadPermission_group,
    loadPermission_userId,

    -- * LoadPermissionModifications
    LoadPermissionModifications (..),
    newLoadPermissionModifications,
    loadPermissionModifications_add,
    loadPermissionModifications_remove,

    -- * LoadPermissionRequest
    LoadPermissionRequest (..),
    newLoadPermissionRequest,
    loadPermissionRequest_group,
    loadPermissionRequest_userId,

    -- * LocalGateway
    LocalGateway (..),
    newLocalGateway,
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_state,
    localGateway_tags,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    newLocalGatewayRoute,
    localGatewayRoute_coipPoolId,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_networkInterfaceId,
    localGatewayRoute_ownerId,
    localGatewayRoute_state,
    localGatewayRoute_subnetId,
    localGatewayRoute_type,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    newLocalGatewayRouteTable,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_mode,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_state,
    localGatewayRouteTable_stateReason,
    localGatewayRouteTable_tags,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- * LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (..),
    newLocalGatewayRouteTableVpcAssociation,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_vpcId,

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    newLocalGatewayVirtualInterface,
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_vlan,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    newLocalGatewayVirtualInterfaceGroup,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_tags,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    newManagedPrefixList,
    managedPrefixList_addressFamily,
    managedPrefixList_maxEntries,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListArn,
    managedPrefixList_prefixListId,
    managedPrefixList_prefixListName,
    managedPrefixList_state,
    managedPrefixList_stateMessage,
    managedPrefixList_tags,
    managedPrefixList_version,

    -- * MemoryGiBPerVCpu
    MemoryGiBPerVCpu (..),
    newMemoryGiBPerVCpu,
    memoryGiBPerVCpu_max,
    memoryGiBPerVCpu_min,

    -- * MemoryGiBPerVCpuRequest
    MemoryGiBPerVCpuRequest (..),
    newMemoryGiBPerVCpuRequest,
    memoryGiBPerVCpuRequest_max,
    memoryGiBPerVCpuRequest_min,

    -- * MemoryInfo
    MemoryInfo (..),
    newMemoryInfo,
    memoryInfo_sizeInMiB,

    -- * MemoryMiB
    MemoryMiB (..),
    newMemoryMiB,
    memoryMiB_max,
    memoryMiB_min,

    -- * MemoryMiBRequest
    MemoryMiBRequest (..),
    newMemoryMiBRequest,
    memoryMiBRequest_max,
    memoryMiBRequest_min,

    -- * MetricPoint
    MetricPoint (..),
    newMetricPoint,
    metricPoint_endDate,
    metricPoint_startDate,
    metricPoint_status,
    metricPoint_value,

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    newModifyTransitGatewayOptions,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_amazonSideAsn,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_vpnEcmpSupport,

    -- * ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (..),
    newModifyTransitGatewayVpcAttachmentRequestOptions,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- * ModifyVerifiedAccessEndpointEniOptions
    ModifyVerifiedAccessEndpointEniOptions (..),
    newModifyVerifiedAccessEndpointEniOptions,
    modifyVerifiedAccessEndpointEniOptions_port,
    modifyVerifiedAccessEndpointEniOptions_protocol,

    -- * ModifyVerifiedAccessEndpointLoadBalancerOptions
    ModifyVerifiedAccessEndpointLoadBalancerOptions (..),
    newModifyVerifiedAccessEndpointLoadBalancerOptions,
    modifyVerifiedAccessEndpointLoadBalancerOptions_port,
    modifyVerifiedAccessEndpointLoadBalancerOptions_protocol,
    modifyVerifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- * ModifyVerifiedAccessTrustProviderOidcOptions
    ModifyVerifiedAccessTrustProviderOidcOptions (..),
    newModifyVerifiedAccessTrustProviderOidcOptions,
    modifyVerifiedAccessTrustProviderOidcOptions_scope,

    -- * ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (..),
    newModifyVpnTunnelOptionsSpecification,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_logOptions,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,

    -- * Monitoring
    Monitoring (..),
    newMonitoring,
    monitoring_state,

    -- * MovingAddressStatus
    MovingAddressStatus (..),
    newMovingAddressStatus,
    movingAddressStatus_moveStatus,
    movingAddressStatus_publicIp,

    -- * NatGateway
    NatGateway (..),
    newNatGateway,
    natGateway_connectivityType,
    natGateway_createTime,
    natGateway_deleteTime,
    natGateway_failureCode,
    natGateway_failureMessage,
    natGateway_natGatewayAddresses,
    natGateway_natGatewayId,
    natGateway_provisionedBandwidth,
    natGateway_state,
    natGateway_subnetId,
    natGateway_tags,
    natGateway_vpcId,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    newNatGatewayAddress,
    natGatewayAddress_allocationId,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_privateIp,
    natGatewayAddress_publicIp,

    -- * NetworkAcl
    NetworkAcl (..),
    newNetworkAcl,
    networkAcl_associations,
    networkAcl_entries,
    networkAcl_isDefault,
    networkAcl_networkAclId,
    networkAcl_ownerId,
    networkAcl_tags,
    networkAcl_vpcId,

    -- * NetworkAclAssociation
    NetworkAclAssociation (..),
    newNetworkAclAssociation,
    networkAclAssociation_networkAclAssociationId,
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,

    -- * NetworkAclEntry
    NetworkAclEntry (..),
    newNetworkAclEntry,
    networkAclEntry_cidrBlock,
    networkAclEntry_egress,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_portRange,
    networkAclEntry_protocol,
    networkAclEntry_ruleAction,
    networkAclEntry_ruleNumber,

    -- * NetworkBandwidthGbps
    NetworkBandwidthGbps (..),
    newNetworkBandwidthGbps,
    networkBandwidthGbps_max,
    networkBandwidthGbps_min,

    -- * NetworkBandwidthGbpsRequest
    NetworkBandwidthGbpsRequest (..),
    newNetworkBandwidthGbpsRequest,
    networkBandwidthGbpsRequest_max,
    networkBandwidthGbpsRequest_min,

    -- * NetworkCardInfo
    NetworkCardInfo (..),
    newNetworkCardInfo,
    networkCardInfo_maximumNetworkInterfaces,
    networkCardInfo_networkCardIndex,
    networkCardInfo_networkPerformance,

    -- * NetworkInfo
    NetworkInfo (..),
    newNetworkInfo,
    networkInfo_defaultNetworkCardIndex,
    networkInfo_efaInfo,
    networkInfo_efaSupported,
    networkInfo_enaSrdSupported,
    networkInfo_enaSupport,
    networkInfo_encryptionInTransitSupported,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_ipv6Supported,
    networkInfo_maximumNetworkCards,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_networkCards,
    networkInfo_networkPerformance,

    -- * NetworkInsightsAccessScope
    NetworkInsightsAccessScope (..),
    newNetworkInsightsAccessScope,
    networkInsightsAccessScope_createdDate,
    networkInsightsAccessScope_networkInsightsAccessScopeArn,
    networkInsightsAccessScope_networkInsightsAccessScopeId,
    networkInsightsAccessScope_tags,
    networkInsightsAccessScope_updatedDate,

    -- * NetworkInsightsAccessScopeAnalysis
    NetworkInsightsAccessScopeAnalysis (..),
    newNetworkInsightsAccessScopeAnalysis,
    networkInsightsAccessScopeAnalysis_analyzedEniCount,
    networkInsightsAccessScopeAnalysis_endDate,
    networkInsightsAccessScopeAnalysis_findingsFound,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    networkInsightsAccessScopeAnalysis_startDate,
    networkInsightsAccessScopeAnalysis_status,
    networkInsightsAccessScopeAnalysis_statusMessage,
    networkInsightsAccessScopeAnalysis_tags,
    networkInsightsAccessScopeAnalysis_warningMessage,

    -- * NetworkInsightsAccessScopeContent
    NetworkInsightsAccessScopeContent (..),
    newNetworkInsightsAccessScopeContent,
    networkInsightsAccessScopeContent_excludePaths,
    networkInsightsAccessScopeContent_matchPaths,
    networkInsightsAccessScopeContent_networkInsightsAccessScopeId,

    -- * NetworkInsightsAnalysis
    NetworkInsightsAnalysis (..),
    newNetworkInsightsAnalysis,
    networkInsightsAnalysis_additionalAccounts,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_suggestedAccounts,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_warningMessage,

    -- * NetworkInsightsPath
    NetworkInsightsPath (..),
    newNetworkInsightsPath,
    networkInsightsPath_createdDate,
    networkInsightsPath_destination,
    networkInsightsPath_destinationArn,
    networkInsightsPath_destinationIp,
    networkInsightsPath_destinationPort,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_protocol,
    networkInsightsPath_source,
    networkInsightsPath_sourceArn,
    networkInsightsPath_sourceIp,
    networkInsightsPath_tags,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_association,
    networkInterface_attachment,
    networkInterface_availabilityZone,
    networkInterface_denyAllIgwTraffic,
    networkInterface_description,
    networkInterface_groups,
    networkInterface_interfaceType,
    networkInterface_ipv4Prefixes,
    networkInterface_ipv6Address,
    networkInterface_ipv6Addresses,
    networkInterface_ipv6Native,
    networkInterface_ipv6Prefixes,
    networkInterface_macAddress,
    networkInterface_networkInterfaceId,
    networkInterface_outpostArn,
    networkInterface_ownerId,
    networkInterface_privateDnsName,
    networkInterface_privateIpAddress,
    networkInterface_privateIpAddresses,
    networkInterface_requesterId,
    networkInterface_requesterManaged,
    networkInterface_sourceDestCheck,
    networkInterface_status,
    networkInterface_subnetId,
    networkInterface_tagSet,
    networkInterface_vpcId,

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    newNetworkInterfaceAssociation,
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_publicIp,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    newNetworkInterfaceAttachment,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_enaSrdSpecification,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_instanceOwnerId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_status,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    newNetworkInterfaceAttachmentChanges,
    networkInterfaceAttachmentChanges_attachmentId,
    networkInterfaceAttachmentChanges_deleteOnTermination,

    -- * NetworkInterfaceCount
    NetworkInterfaceCount (..),
    newNetworkInterfaceCount,
    networkInterfaceCount_max,
    networkInterfaceCount_min,

    -- * NetworkInterfaceCountRequest
    NetworkInterfaceCountRequest (..),
    newNetworkInterfaceCountRequest,
    networkInterfaceCountRequest_max,
    networkInterfaceCountRequest_min,

    -- * NetworkInterfaceIpv6Address
    NetworkInterfaceIpv6Address (..),
    newNetworkInterfaceIpv6Address,
    networkInterfaceIpv6Address_ipv6Address,

    -- * NetworkInterfacePermission
    NetworkInterfacePermission (..),
    newNetworkInterfacePermission,
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_awsService,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_permission,
    networkInterfacePermission_permissionState,

    -- * NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    newNetworkInterfacePermissionState,
    networkInterfacePermissionState_state,
    networkInterfacePermissionState_statusMessage,

    -- * NetworkInterfacePrivateIpAddress
    NetworkInterfacePrivateIpAddress (..),
    newNetworkInterfacePrivateIpAddress,
    networkInterfacePrivateIpAddress_association,
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_privateIpAddress,

    -- * NewDhcpConfiguration
    NewDhcpConfiguration (..),
    newNewDhcpConfiguration,
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- * OidcOptions
    OidcOptions (..),
    newOidcOptions,
    oidcOptions_authorizationEndpoint,
    oidcOptions_clientId,
    oidcOptions_clientSecret,
    oidcOptions_issuer,
    oidcOptions_scope,
    oidcOptions_tokenEndpoint,
    oidcOptions_userInfoEndpoint,

    -- * OnDemandOptions
    OnDemandOptions (..),
    newOnDemandOptions,
    onDemandOptions_allocationStrategy,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_singleInstanceType,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    newOnDemandOptionsRequest,
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_singleInstanceType,

    -- * PacketHeaderStatement
    PacketHeaderStatement (..),
    newPacketHeaderStatement,
    packetHeaderStatement_destinationAddresses,
    packetHeaderStatement_destinationPorts,
    packetHeaderStatement_destinationPrefixLists,
    packetHeaderStatement_protocols,
    packetHeaderStatement_sourceAddresses,
    packetHeaderStatement_sourcePorts,
    packetHeaderStatement_sourcePrefixLists,

    -- * PacketHeaderStatementRequest
    PacketHeaderStatementRequest (..),
    newPacketHeaderStatementRequest,
    packetHeaderStatementRequest_destinationAddresses,
    packetHeaderStatementRequest_destinationPorts,
    packetHeaderStatementRequest_destinationPrefixLists,
    packetHeaderStatementRequest_protocols,
    packetHeaderStatementRequest_sourceAddresses,
    packetHeaderStatementRequest_sourcePorts,
    packetHeaderStatementRequest_sourcePrefixLists,

    -- * PathComponent
    PathComponent (..),
    newPathComponent,
    pathComponent_aclRule,
    pathComponent_additionalDetails,
    pathComponent_attachedTo,
    pathComponent_component,
    pathComponent_destinationVpc,
    pathComponent_elasticLoadBalancerListener,
    pathComponent_explanations,
    pathComponent_inboundHeader,
    pathComponent_outboundHeader,
    pathComponent_routeTableRoute,
    pathComponent_securityGroupRule,
    pathComponent_sequenceNumber,
    pathComponent_sourceVpc,
    pathComponent_subnet,
    pathComponent_transitGateway,
    pathComponent_transitGatewayRouteTableRoute,
    pathComponent_vpc,

    -- * PathStatement
    PathStatement (..),
    newPathStatement,
    pathStatement_packetHeaderStatement,
    pathStatement_resourceStatement,

    -- * PathStatementRequest
    PathStatementRequest (..),
    newPathStatementRequest,
    pathStatementRequest_packetHeaderStatement,
    pathStatementRequest_resourceStatement,

    -- * PciId
    PciId (..),
    newPciId,
    pciId_deviceId,
    pciId_subsystemId,
    pciId_subsystemVendorId,
    pciId_vendorId,

    -- * PeeringAttachmentStatus
    PeeringAttachmentStatus (..),
    newPeeringAttachmentStatus,
    peeringAttachmentStatus_code,
    peeringAttachmentStatus_message,

    -- * PeeringConnectionOptions
    PeeringConnectionOptions (..),
    newPeeringConnectionOptions,
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,

    -- * PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    newPeeringConnectionOptionsRequest,
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,

    -- * PeeringTgwInfo
    PeeringTgwInfo (..),
    newPeeringTgwInfo,
    peeringTgwInfo_coreNetworkId,
    peeringTgwInfo_ownerId,
    peeringTgwInfo_region,
    peeringTgwInfo_transitGatewayId,

    -- * Phase1DHGroupNumbersListValue
    Phase1DHGroupNumbersListValue (..),
    newPhase1DHGroupNumbersListValue,
    phase1DHGroupNumbersListValue_value,

    -- * Phase1DHGroupNumbersRequestListValue
    Phase1DHGroupNumbersRequestListValue (..),
    newPhase1DHGroupNumbersRequestListValue,
    phase1DHGroupNumbersRequestListValue_value,

    -- * Phase1EncryptionAlgorithmsListValue
    Phase1EncryptionAlgorithmsListValue (..),
    newPhase1EncryptionAlgorithmsListValue,
    phase1EncryptionAlgorithmsListValue_value,

    -- * Phase1EncryptionAlgorithmsRequestListValue
    Phase1EncryptionAlgorithmsRequestListValue (..),
    newPhase1EncryptionAlgorithmsRequestListValue,
    phase1EncryptionAlgorithmsRequestListValue_value,

    -- * Phase1IntegrityAlgorithmsListValue
    Phase1IntegrityAlgorithmsListValue (..),
    newPhase1IntegrityAlgorithmsListValue,
    phase1IntegrityAlgorithmsListValue_value,

    -- * Phase1IntegrityAlgorithmsRequestListValue
    Phase1IntegrityAlgorithmsRequestListValue (..),
    newPhase1IntegrityAlgorithmsRequestListValue,
    phase1IntegrityAlgorithmsRequestListValue_value,

    -- * Phase2DHGroupNumbersListValue
    Phase2DHGroupNumbersListValue (..),
    newPhase2DHGroupNumbersListValue,
    phase2DHGroupNumbersListValue_value,

    -- * Phase2DHGroupNumbersRequestListValue
    Phase2DHGroupNumbersRequestListValue (..),
    newPhase2DHGroupNumbersRequestListValue,
    phase2DHGroupNumbersRequestListValue_value,

    -- * Phase2EncryptionAlgorithmsListValue
    Phase2EncryptionAlgorithmsListValue (..),
    newPhase2EncryptionAlgorithmsListValue,
    phase2EncryptionAlgorithmsListValue_value,

    -- * Phase2EncryptionAlgorithmsRequestListValue
    Phase2EncryptionAlgorithmsRequestListValue (..),
    newPhase2EncryptionAlgorithmsRequestListValue,
    phase2EncryptionAlgorithmsRequestListValue_value,

    -- * Phase2IntegrityAlgorithmsListValue
    Phase2IntegrityAlgorithmsListValue (..),
    newPhase2IntegrityAlgorithmsListValue,
    phase2IntegrityAlgorithmsListValue_value,

    -- * Phase2IntegrityAlgorithmsRequestListValue
    Phase2IntegrityAlgorithmsRequestListValue (..),
    newPhase2IntegrityAlgorithmsRequestListValue,
    phase2IntegrityAlgorithmsRequestListValue_value,

    -- * Placement
    Placement (..),
    newPlacement,
    placement_affinity,
    placement_availabilityZone,
    placement_groupId,
    placement_groupName,
    placement_hostId,
    placement_hostResourceGroupArn,
    placement_partitionNumber,
    placement_spreadDomain,
    placement_tenancy,

    -- * PlacementGroup
    PlacementGroup (..),
    newPlacementGroup,
    placementGroup_groupArn,
    placementGroup_groupId,
    placementGroup_groupName,
    placementGroup_partitionCount,
    placementGroup_spreadLevel,
    placementGroup_state,
    placementGroup_strategy,
    placementGroup_tags,

    -- * PlacementGroupInfo
    PlacementGroupInfo (..),
    newPlacementGroupInfo,
    placementGroupInfo_supportedStrategies,

    -- * PlacementResponse
    PlacementResponse (..),
    newPlacementResponse,
    placementResponse_groupName,

    -- * PoolCidrBlock
    PoolCidrBlock (..),
    newPoolCidrBlock,
    poolCidrBlock_cidr,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_from,
    portRange_to,

    -- * PrefixList
    PrefixList (..),
    newPrefixList,
    prefixList_cidrs,
    prefixList_prefixListId,
    prefixList_prefixListName,

    -- * PrefixListAssociation
    PrefixListAssociation (..),
    newPrefixListAssociation,
    prefixListAssociation_resourceId,
    prefixListAssociation_resourceOwner,

    -- * PrefixListEntry
    PrefixListEntry (..),
    newPrefixListEntry,
    prefixListEntry_cidr,
    prefixListEntry_description,

    -- * PrefixListId
    PrefixListId (..),
    newPrefixListId,
    prefixListId_description,
    prefixListId_prefixListId,

    -- * PriceSchedule
    PriceSchedule (..),
    newPriceSchedule,
    priceSchedule_active,
    priceSchedule_currencyCode,
    priceSchedule_price,
    priceSchedule_term,

    -- * PriceScheduleSpecification
    PriceScheduleSpecification (..),
    newPriceScheduleSpecification,
    priceScheduleSpecification_currencyCode,
    priceScheduleSpecification_price,
    priceScheduleSpecification_term,

    -- * PricingDetail
    PricingDetail (..),
    newPricingDetail,
    pricingDetail_count,
    pricingDetail_price,

    -- * PrincipalIdFormat
    PrincipalIdFormat (..),
    newPrincipalIdFormat,
    principalIdFormat_arn,
    principalIdFormat_statuses,

    -- * PrivateDnsDetails
    PrivateDnsDetails (..),
    newPrivateDnsDetails,
    privateDnsDetails_privateDnsName,

    -- * PrivateDnsNameConfiguration
    PrivateDnsNameConfiguration (..),
    newPrivateDnsNameConfiguration,
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_type,
    privateDnsNameConfiguration_value,

    -- * PrivateDnsNameOptionsOnLaunch
    PrivateDnsNameOptionsOnLaunch (..),
    newPrivateDnsNameOptionsOnLaunch,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord,
    privateDnsNameOptionsOnLaunch_hostnameType,

    -- * PrivateDnsNameOptionsRequest
    PrivateDnsNameOptionsRequest (..),
    newPrivateDnsNameOptionsRequest,
    privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    privateDnsNameOptionsRequest_hostnameType,

    -- * PrivateDnsNameOptionsResponse
    PrivateDnsNameOptionsResponse (..),
    newPrivateDnsNameOptionsResponse,
    privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsResponse_enableResourceNameDnsARecord,
    privateDnsNameOptionsResponse_hostnameType,

    -- * PrivateIpAddressSpecification
    PrivateIpAddressSpecification (..),
    newPrivateIpAddressSpecification,
    privateIpAddressSpecification_primary,
    privateIpAddressSpecification_privateIpAddress,

    -- * ProcessorInfo
    ProcessorInfo (..),
    newProcessorInfo,
    processorInfo_supportedArchitectures,
    processorInfo_sustainedClockSpeedInGhz,

    -- * ProductCode
    ProductCode (..),
    newProductCode,
    productCode_productCodeId,
    productCode_productCodeType,

    -- * PropagatingVgw
    PropagatingVgw (..),
    newPropagatingVgw,
    propagatingVgw_gatewayId,

    -- * ProvisionedBandwidth
    ProvisionedBandwidth (..),
    newProvisionedBandwidth,
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_provisioned,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_requested,
    provisionedBandwidth_status,

    -- * PtrUpdateStatus
    PtrUpdateStatus (..),
    newPtrUpdateStatus,
    ptrUpdateStatus_reason,
    ptrUpdateStatus_status,
    ptrUpdateStatus_value,

    -- * PublicIpv4Pool
    PublicIpv4Pool (..),
    newPublicIpv4Pool,
    publicIpv4Pool_description,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_poolId,
    publicIpv4Pool_tags,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_totalAvailableAddressCount,

    -- * PublicIpv4PoolRange
    PublicIpv4PoolRange (..),
    newPublicIpv4PoolRange,
    publicIpv4PoolRange_addressCount,
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,

    -- * Purchase
    Purchase (..),
    newPurchase,
    purchase_currencyCode,
    purchase_duration,
    purchase_hostIdSet,
    purchase_hostReservationId,
    purchase_hourlyPrice,
    purchase_instanceFamily,
    purchase_paymentOption,
    purchase_upfrontPrice,

    -- * PurchaseRequest
    PurchaseRequest (..),
    newPurchaseRequest,
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_amount,
    recurringCharge_frequency,

    -- * ReferencedSecurityGroup
    ReferencedSecurityGroup (..),
    newReferencedSecurityGroup,
    referencedSecurityGroup_groupId,
    referencedSecurityGroup_peeringStatus,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_vpcPeeringConnectionId,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_endpoint,
    regionInfo_optInStatus,
    regionInfo_regionName,

    -- * RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    newRegisterInstanceTagAttributeRequest,
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,
    registerInstanceTagAttributeRequest_instanceTagKeys,

    -- * RemoveIpamOperatingRegion
    RemoveIpamOperatingRegion (..),
    newRemoveIpamOperatingRegion,
    removeIpamOperatingRegion_regionName,

    -- * RemovePrefixListEntry
    RemovePrefixListEntry (..),
    newRemovePrefixListEntry,
    removePrefixListEntry_cidr,

    -- * ReplaceRootVolumeTask
    ReplaceRootVolumeTask (..),
    newReplaceRootVolumeTask,
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_deleteReplacedRootVolume,
    replaceRootVolumeTask_imageId,
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_snapshotId,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_tags,
    replaceRootVolumeTask_taskState,

    -- * RequestIpamResourceTag
    RequestIpamResourceTag (..),
    newRequestIpamResourceTag,
    requestIpamResourceTag_key,
    requestIpamResourceTag_value,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    newRequestLaunchTemplateData,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_disableApiStop,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_instanceRequirements,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_maintenanceOptions,
    requestLaunchTemplateData_metadataOptions,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_privateDnsNameOptions,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_userData,

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    newRequestSpotLaunchSpecification,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_networkInterfaces,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_userData,

    -- * Reservation
    Reservation (..),
    newReservation,
    reservation_groups,
    reservation_instances,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- * ReservationFleetInstanceSpecification
    ReservationFleetInstanceSpecification (..),
    newReservationFleetInstanceSpecification,
    reservationFleetInstanceSpecification_availabilityZone,
    reservationFleetInstanceSpecification_availabilityZoneId,
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_weight,

    -- * ReservationValue
    ReservationValue (..),
    newReservationValue,
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,
    reservationValue_remainingUpfrontValue,

    -- * ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (..),
    newReservedInstanceLimitPrice,
    reservedInstanceLimitPrice_amount,
    reservedInstanceLimitPrice_currencyCode,

    -- * ReservedInstanceReservationValue
    ReservedInstanceReservationValue (..),
    newReservedInstanceReservationValue,
    reservedInstanceReservationValue_reservationValue,
    reservedInstanceReservationValue_reservedInstanceId,

    -- * ReservedInstances
    ReservedInstances (..),
    newReservedInstances,
    reservedInstances_availabilityZone,
    reservedInstances_currencyCode,
    reservedInstances_duration,
    reservedInstances_end,
    reservedInstances_fixedPrice,
    reservedInstances_instanceCount,
    reservedInstances_instanceTenancy,
    reservedInstances_instanceType,
    reservedInstances_offeringClass,
    reservedInstances_offeringType,
    reservedInstances_productDescription,
    reservedInstances_recurringCharges,
    reservedInstances_reservedInstancesId,
    reservedInstances_scope,
    reservedInstances_start,
    reservedInstances_state,
    reservedInstances_tags,
    reservedInstances_usagePrice,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    newReservedInstancesConfiguration,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceCount,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_scope,

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    newReservedInstancesId,
    reservedInstancesId_reservedInstancesId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    newReservedInstancesListing,
    reservedInstancesListing_clientToken,
    reservedInstancesListing_createDate,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_status,
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_tags,
    reservedInstancesListing_updateDate,

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    newReservedInstancesModification,
    reservedInstancesModification_clientToken,
    reservedInstancesModification_createDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_status,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_updateDate,

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    newReservedInstancesModificationResult,
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    newReservedInstancesOffering,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_usagePrice,

    -- * ResourceStatement
    ResourceStatement (..),
    newResourceStatement,
    resourceStatement_resourceTypes,
    resourceStatement_resources,

    -- * ResourceStatementRequest
    ResourceStatementRequest (..),
    newResourceStatementRequest,
    resourceStatementRequest_resourceTypes,
    resourceStatementRequest_resources,

    -- * ResponseError
    ResponseError (..),
    newResponseError,
    responseError_code,
    responseError_message,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    newResponseLaunchTemplateData,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_disableApiStop,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_instanceRequirements,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_maintenanceOptions,
    responseLaunchTemplateData_metadataOptions,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_privateDnsNameOptions,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_userData,

    -- * Route
    Route (..),
    newRoute,
    route_carrierGatewayId,
    route_coreNetworkArn,
    route_destinationCidrBlock,
    route_destinationIpv6CidrBlock,
    route_destinationPrefixListId,
    route_egressOnlyInternetGatewayId,
    route_gatewayId,
    route_instanceId,
    route_instanceOwnerId,
    route_localGatewayId,
    route_natGatewayId,
    route_networkInterfaceId,
    route_origin,
    route_state,
    route_transitGatewayId,
    route_vpcPeeringConnectionId,

    -- * RouteTable
    RouteTable (..),
    newRouteTable,
    routeTable_associations,
    routeTable_ownerId,
    routeTable_propagatingVgws,
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_tags,
    routeTable_vpcId,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    newRouteTableAssociation,
    routeTableAssociation_associationState,
    routeTableAssociation_gatewayId,
    routeTableAssociation_main,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_routeTableId,
    routeTableAssociation_subnetId,

    -- * RouteTableAssociationState
    RouteTableAssociationState (..),
    newRouteTableAssociationState,
    routeTableAssociationState_state,
    routeTableAssociationState_statusMessage,

    -- * RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (..),
    newRunInstancesMonitoringEnabled,
    runInstancesMonitoringEnabled_enabled,

    -- * S3ObjectTag
    S3ObjectTag (..),
    newS3ObjectTag,
    s3ObjectTag_key,
    s3ObjectTag_value,

    -- * S3Storage
    S3Storage (..),
    newS3Storage,
    s3Storage_aWSAccessKeyId,
    s3Storage_bucket,
    s3Storage_prefix,
    s3Storage_uploadPolicy,
    s3Storage_uploadPolicySignature,

    -- * ScheduledInstance
    ScheduledInstance (..),
    newScheduledInstance,
    scheduledInstance_availabilityZone,
    scheduledInstance_createDate,
    scheduledInstance_hourlyPrice,
    scheduledInstance_instanceCount,
    scheduledInstance_instanceType,
    scheduledInstance_networkPlatform,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_platform,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_recurrence,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_termEndDate,
    scheduledInstance_termStartDate,
    scheduledInstance_totalScheduledInstanceHours,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    newScheduledInstanceAvailability,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_totalScheduledInstanceHours,

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    newScheduledInstanceRecurrence,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceDaySet,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    newScheduledInstanceRecurrenceRequest,
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    newScheduledInstancesBlockDeviceMapping,
    scheduledInstancesBlockDeviceMapping_deviceName,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,

    -- * ScheduledInstancesEbs
    ScheduledInstancesEbs (..),
    newScheduledInstancesEbs,
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_iops,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_volumeType,

    -- * ScheduledInstancesIamInstanceProfile
    ScheduledInstancesIamInstanceProfile (..),
    newScheduledInstancesIamInstanceProfile,
    scheduledInstancesIamInstanceProfile_arn,
    scheduledInstancesIamInstanceProfile_name,

    -- * ScheduledInstancesIpv6Address
    ScheduledInstancesIpv6Address (..),
    newScheduledInstancesIpv6Address,
    scheduledInstancesIpv6Address_ipv6Address,

    -- * ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (..),
    newScheduledInstancesLaunchSpecification,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_imageId,

    -- * ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    newScheduledInstancesMonitoring,
    scheduledInstancesMonitoring_enabled,

    -- * ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    newScheduledInstancesNetworkInterface,
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_subnetId,

    -- * ScheduledInstancesPlacement
    ScheduledInstancesPlacement (..),
    newScheduledInstancesPlacement,
    scheduledInstancesPlacement_availabilityZone,
    scheduledInstancesPlacement_groupName,

    -- * ScheduledInstancesPrivateIpAddressConfig
    ScheduledInstancesPrivateIpAddressConfig (..),
    newScheduledInstancesPrivateIpAddressConfig,
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- * SecurityGroup
    SecurityGroup (..),
    newSecurityGroup,
    securityGroup_ipPermissions,
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
    securityGroup_vpcId,
    securityGroup_ownerId,
    securityGroup_groupId,
    securityGroup_groupName,
    securityGroup_description,

    -- * SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    newSecurityGroupIdentifier,
    securityGroupIdentifier_groupId,
    securityGroupIdentifier_groupName,

    -- * SecurityGroupReference
    SecurityGroupReference (..),
    newSecurityGroupReference,
    securityGroupReference_groupId,
    securityGroupReference_referencingVpcId,
    securityGroupReference_vpcPeeringConnectionId,

    -- * SecurityGroupRule
    SecurityGroupRule (..),
    newSecurityGroupRule,
    securityGroupRule_cidrIpv4,
    securityGroupRule_cidrIpv6,
    securityGroupRule_description,
    securityGroupRule_fromPort,
    securityGroupRule_groupId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_ipProtocol,
    securityGroupRule_isEgress,
    securityGroupRule_prefixListId,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_tags,
    securityGroupRule_toPort,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_description,
    securityGroupRuleDescription_securityGroupRuleId,

    -- * SecurityGroupRuleRequest
    SecurityGroupRuleRequest (..),
    newSecurityGroupRuleRequest,
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_description,
    securityGroupRuleRequest_fromPort,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_toPort,

    -- * SecurityGroupRuleUpdate
    SecurityGroupRuleUpdate (..),
    newSecurityGroupRuleUpdate,
    securityGroupRuleUpdate_securityGroupRule,
    securityGroupRuleUpdate_securityGroupRuleId,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    newServiceConfiguration,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_payerResponsibility,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_serviceId,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceType,
    serviceConfiguration_supportedIpAddressTypes,
    serviceConfiguration_tags,

    -- * ServiceDetail
    ServiceDetail (..),
    newServiceDetail,
    serviceDetail_acceptanceRequired,
    serviceDetail_availabilityZones,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_owner,
    serviceDetail_payerResponsibility,
    serviceDetail_privateDnsName,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_privateDnsNames,
    serviceDetail_serviceId,
    serviceDetail_serviceName,
    serviceDetail_serviceType,
    serviceDetail_supportedIpAddressTypes,
    serviceDetail_tags,
    serviceDetail_vpcEndpointPolicySupported,

    -- * ServiceTypeDetail
    ServiceTypeDetail (..),
    newServiceTypeDetail,
    serviceTypeDetail_serviceType,

    -- * SlotDateTimeRangeRequest
    SlotDateTimeRangeRequest (..),
    newSlotDateTimeRangeRequest,
    slotDateTimeRangeRequest_earliestTime,
    slotDateTimeRangeRequest_latestTime,

    -- * SlotStartTimeRangeRequest
    SlotStartTimeRangeRequest (..),
    newSlotStartTimeRangeRequest,
    slotStartTimeRangeRequest_earliestTime,
    slotStartTimeRangeRequest_latestTime,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_outpostArn,
    snapshot_ownerAlias,
    snapshot_restoreExpiryTime,
    snapshot_stateMessage,
    snapshot_storageTier,
    snapshot_tags,
    snapshot_snapshotId,
    snapshot_ownerId,
    snapshot_volumeId,
    snapshot_volumeSize,
    snapshot_description,
    snapshot_startTime,
    snapshot_progress,
    snapshot_state,
    snapshot_encrypted,

    -- * SnapshotDetail
    SnapshotDetail (..),
    newSnapshotDetail,
    snapshotDetail_description,
    snapshotDetail_deviceName,
    snapshotDetail_diskImageSize,
    snapshotDetail_format,
    snapshotDetail_progress,
    snapshotDetail_snapshotId,
    snapshotDetail_status,
    snapshotDetail_statusMessage,
    snapshotDetail_url,
    snapshotDetail_userBucket,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    newSnapshotDiskContainer,
    snapshotDiskContainer_description,
    snapshotDiskContainer_format,
    snapshotDiskContainer_url,
    snapshotDiskContainer_userBucket,

    -- * SnapshotInfo
    SnapshotInfo (..),
    newSnapshotInfo,
    snapshotInfo_description,
    snapshotInfo_encrypted,
    snapshotInfo_outpostArn,
    snapshotInfo_ownerId,
    snapshotInfo_progress,
    snapshotInfo_snapshotId,
    snapshotInfo_startTime,
    snapshotInfo_state,
    snapshotInfo_tags,
    snapshotInfo_volumeId,
    snapshotInfo_volumeSize,

    -- * SnapshotRecycleBinInfo
    SnapshotRecycleBinInfo (..),
    newSnapshotRecycleBinInfo,
    snapshotRecycleBinInfo_description,
    snapshotRecycleBinInfo_recycleBinEnterTime,
    snapshotRecycleBinInfo_recycleBinExitTime,
    snapshotRecycleBinInfo_snapshotId,
    snapshotRecycleBinInfo_volumeId,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    newSnapshotTaskDetail,
    snapshotTaskDetail_description,
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_format,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_progress,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_status,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_url,
    snapshotTaskDetail_userBucket,

    -- * SnapshotTierStatus
    SnapshotTierStatus (..),
    newSnapshotTierStatus,
    snapshotTierStatus_archivalCompleteTime,
    snapshotTierStatus_lastTieringOperationStatus,
    snapshotTierStatus_lastTieringOperationStatusDetail,
    snapshotTierStatus_lastTieringProgress,
    snapshotTierStatus_lastTieringStartTime,
    snapshotTierStatus_ownerId,
    snapshotTierStatus_restoreExpiryTime,
    snapshotTierStatus_snapshotId,
    snapshotTierStatus_status,
    snapshotTierStatus_storageTier,
    snapshotTierStatus_tags,
    snapshotTierStatus_volumeId,

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    newSpotCapacityRebalance,
    spotCapacityRebalance_replacementStrategy,
    spotCapacityRebalance_terminationDelay,

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    newSpotDatafeedSubscription,
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_state,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    newSpotFleetLaunchSpecification,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_instanceRequirements,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_weightedCapacity,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    newSpotFleetMonitoring,
    spotFleetMonitoring_enabled,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    newSpotFleetRequestConfig,
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_tags,

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    newSpotFleetRequestConfigData,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_targetCapacityUnitType,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- * SpotFleetTagSpecification
    SpotFleetTagSpecification (..),
    newSpotFleetTagSpecification,
    spotFleetTagSpecification_resourceType,
    spotFleetTagSpecification_tags,

    -- * SpotInstanceRequest
    SpotInstanceRequest (..),
    newSpotInstanceRequest,
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_createTime,
    spotInstanceRequest_fault,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_state,
    spotInstanceRequest_status,
    spotInstanceRequest_tags,
    spotInstanceRequest_type,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_validUntil,

    -- * SpotInstanceStateFault
    SpotInstanceStateFault (..),
    newSpotInstanceStateFault,
    spotInstanceStateFault_code,
    spotInstanceStateFault_message,

    -- * SpotInstanceStatus
    SpotInstanceStatus (..),
    newSpotInstanceStatus,
    spotInstanceStatus_code,
    spotInstanceStatus_message,
    spotInstanceStatus_updateTime,

    -- * SpotMaintenanceStrategies
    SpotMaintenanceStrategies (..),
    newSpotMaintenanceStrategies,
    spotMaintenanceStrategies_capacityRebalance,

    -- * SpotMarketOptions
    SpotMarketOptions (..),
    newSpotMarketOptions,
    spotMarketOptions_blockDurationMinutes,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_maxPrice,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_validUntil,

    -- * SpotOptions
    SpotOptions (..),
    newSpotOptions,
    spotOptions_allocationStrategy,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_instancePoolsToUseCount,
    spotOptions_maintenanceStrategies,
    spotOptions_maxTotalPrice,
    spotOptions_minTargetCapacity,
    spotOptions_singleAvailabilityZone,
    spotOptions_singleInstanceType,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    newSpotOptionsRequest,
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_singleInstanceType,

    -- * SpotPlacement
    SpotPlacement (..),
    newSpotPlacement,
    spotPlacement_availabilityZone,
    spotPlacement_groupName,
    spotPlacement_tenancy,

    -- * SpotPlacementScore
    SpotPlacementScore (..),
    newSpotPlacementScore,
    spotPlacementScore_availabilityZoneId,
    spotPlacementScore_region,
    spotPlacementScore_score,

    -- * SpotPrice
    SpotPrice (..),
    newSpotPrice,
    spotPrice_availabilityZone,
    spotPrice_instanceType,
    spotPrice_productDescription,
    spotPrice_spotPrice,
    spotPrice_timestamp,

    -- * StaleIpPermission
    StaleIpPermission (..),
    newStaleIpPermission,
    staleIpPermission_fromPort,
    staleIpPermission_ipProtocol,
    staleIpPermission_ipRanges,
    staleIpPermission_prefixListIds,
    staleIpPermission_toPort,
    staleIpPermission_userIdGroupPairs,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    newStaleSecurityGroup,
    staleSecurityGroup_description,
    staleSecurityGroup_groupId,
    staleSecurityGroup_groupName,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,

    -- * StateReason
    StateReason (..),
    newStateReason,
    stateReason_code,
    stateReason_message,

    -- * Storage
    Storage (..),
    newStorage,
    storage_s3,

    -- * StorageLocation
    StorageLocation (..),
    newStorageLocation,
    storageLocation_bucket,
    storageLocation_key,

    -- * StoreImageTaskResult
    StoreImageTaskResult (..),
    newStoreImageTaskResult,
    storeImageTaskResult_amiId,
    storeImageTaskResult_bucket,
    storeImageTaskResult_progressPercentage,
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskFailureReason,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_taskStartTime,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_assignIpv6AddressOnCreation,
    subnet_availabilityZoneId,
    subnet_customerOwnedIpv4Pool,
    subnet_defaultForAz,
    subnet_enableDns64,
    subnet_enableLniAtDeviceIndex,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_ipv6Native,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_mapPublicIpOnLaunch,
    subnet_outpostArn,
    subnet_ownerId,
    subnet_privateDnsNameOptionsOnLaunch,
    subnet_subnetArn,
    subnet_tags,
    subnet_availabilityZone,
    subnet_availableIpAddressCount,
    subnet_cidrBlock,
    subnet_state,
    subnet_subnetId,
    subnet_vpcId,

    -- * SubnetAssociation
    SubnetAssociation (..),
    newSubnetAssociation,
    subnetAssociation_state,
    subnetAssociation_subnetId,

    -- * SubnetCidrBlockState
    SubnetCidrBlockState (..),
    newSubnetCidrBlockState,
    subnetCidrBlockState_state,
    subnetCidrBlockState_statusMessage,

    -- * SubnetCidrReservation
    SubnetCidrReservation (..),
    newSubnetCidrReservation,
    subnetCidrReservation_cidr,
    subnetCidrReservation_description,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_subnetId,
    subnetCidrReservation_tags,

    -- * SubnetIpv6CidrBlockAssociation
    SubnetIpv6CidrBlockAssociation (..),
    newSubnetIpv6CidrBlockAssociation,
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_destination,
    subscription_metric,
    subscription_period,
    subscription_source,
    subscription_statistic,

    -- * SuccessfulInstanceCreditSpecificationItem
    SuccessfulInstanceCreditSpecificationItem (..),
    newSuccessfulInstanceCreditSpecificationItem,
    successfulInstanceCreditSpecificationItem_instanceId,

    -- * SuccessfulQueuedPurchaseDeletion
    SuccessfulQueuedPurchaseDeletion (..),
    newSuccessfulQueuedPurchaseDeletion,
    successfulQueuedPurchaseDeletion_reservedInstancesId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagDescription
    TagDescription (..),
    newTagDescription,
    tagDescription_resourceId,
    tagDescription_resourceType,
    tagDescription_key,
    tagDescription_value,

    -- * TagSpecification
    TagSpecification (..),
    newTagSpecification,
    tagSpecification_resourceType,
    tagSpecification_tags,

    -- * TargetCapacitySpecification
    TargetCapacitySpecification (..),
    newTargetCapacitySpecification,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,
    targetCapacitySpecification_targetCapacityUnitType,
    targetCapacitySpecification_totalTargetCapacity,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    newTargetCapacitySpecificationRequest,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_spotTargetCapacity,
    targetCapacitySpecificationRequest_targetCapacityUnitType,
    targetCapacitySpecificationRequest_totalTargetCapacity,

    -- * TargetConfiguration
    TargetConfiguration (..),
    newTargetConfiguration,
    targetConfiguration_instanceCount,
    targetConfiguration_offeringId,

    -- * TargetConfigurationRequest
    TargetConfigurationRequest (..),
    newTargetConfigurationRequest,
    targetConfigurationRequest_instanceCount,
    targetConfigurationRequest_offeringId,

    -- * TargetGroup
    TargetGroup (..),
    newTargetGroup,
    targetGroup_arn,

    -- * TargetGroupsConfig
    TargetGroupsConfig (..),
    newTargetGroupsConfig,
    targetGroupsConfig_targetGroups,

    -- * TargetNetwork
    TargetNetwork (..),
    newTargetNetwork,
    targetNetwork_associationId,
    targetNetwork_clientVpnEndpointId,
    targetNetwork_securityGroups,
    targetNetwork_status,
    targetNetwork_targetNetworkId,
    targetNetwork_vpcId,

    -- * TargetReservationValue
    TargetReservationValue (..),
    newTargetReservationValue,
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    newTerminateConnectionStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_currentStatus,
    terminateConnectionStatus_previousStatus,

    -- * ThroughResourcesStatement
    ThroughResourcesStatement (..),
    newThroughResourcesStatement,
    throughResourcesStatement_resourceStatement,

    -- * ThroughResourcesStatementRequest
    ThroughResourcesStatementRequest (..),
    newThroughResourcesStatementRequest,
    throughResourcesStatementRequest_resourceStatement,

    -- * TotalLocalStorageGB
    TotalLocalStorageGB (..),
    newTotalLocalStorageGB,
    totalLocalStorageGB_max,
    totalLocalStorageGB_min,

    -- * TotalLocalStorageGBRequest
    TotalLocalStorageGBRequest (..),
    newTotalLocalStorageGBRequest,
    totalLocalStorageGBRequest_max,
    totalLocalStorageGBRequest_min,

    -- * TrafficMirrorFilter
    TrafficMirrorFilter (..),
    newTrafficMirrorFilter,
    trafficMirrorFilter_description,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_tags,
    trafficMirrorFilter_trafficMirrorFilterId,

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    newTrafficMirrorFilterRule,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,

    -- * TrafficMirrorPortRange
    TrafficMirrorPortRange (..),
    newTrafficMirrorPortRange,
    trafficMirrorPortRange_fromPort,
    trafficMirrorPortRange_toPort,

    -- * TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (..),
    newTrafficMirrorPortRangeRequest,
    trafficMirrorPortRangeRequest_fromPort,
    trafficMirrorPortRangeRequest_toPort,

    -- * TrafficMirrorSession
    TrafficMirrorSession (..),
    newTrafficMirrorSession,
    trafficMirrorSession_description,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_virtualNetworkId,

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    newTrafficMirrorTarget,
    trafficMirrorTarget_description,
    trafficMirrorTarget_gatewayLoadBalancerEndpointId,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_tags,
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_type,

    -- * TransitGateway
    TransitGateway (..),
    newTransitGateway,
    transitGateway_creationTime,
    transitGateway_description,
    transitGateway_options,
    transitGateway_ownerId,
    transitGateway_state,
    transitGateway_tags,
    transitGateway_transitGatewayArn,
    transitGateway_transitGatewayId,

    -- * TransitGatewayAssociation
    TransitGatewayAssociation (..),
    newTransitGatewayAssociation,
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_state,
    transitGatewayAssociation_transitGatewayAttachmentId,
    transitGatewayAssociation_transitGatewayRouteTableId,

    -- * TransitGatewayAttachment
    TransitGatewayAttachment (..),
    newTransitGatewayAttachment,
    transitGatewayAttachment_association,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_state,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_transitGatewayOwnerId,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    newTransitGatewayAttachmentAssociation,
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- * TransitGatewayAttachmentBgpConfiguration
    TransitGatewayAttachmentBgpConfiguration (..),
    newTransitGatewayAttachmentBgpConfiguration,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,

    -- * TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    newTransitGatewayAttachmentPropagation,
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- * TransitGatewayConnect
    TransitGatewayConnect (..),
    newTransitGatewayConnect,
    transitGatewayConnect_creationTime,
    transitGatewayConnect_options,
    transitGatewayConnect_state,
    transitGatewayConnect_tags,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_transportTransitGatewayAttachmentId,

    -- * TransitGatewayConnectOptions
    TransitGatewayConnectOptions (..),
    newTransitGatewayConnectOptions,
    transitGatewayConnectOptions_protocol,

    -- * TransitGatewayConnectPeer
    TransitGatewayConnectPeer (..),
    newTransitGatewayConnectPeer,
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,

    -- * TransitGatewayConnectPeerConfiguration
    TransitGatewayConnectPeerConfiguration (..),
    newTransitGatewayConnectPeerConfiguration,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,

    -- * TransitGatewayConnectRequestBgpOptions
    TransitGatewayConnectRequestBgpOptions (..),
    newTransitGatewayConnectRequestBgpOptions,
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- * TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    newTransitGatewayMulticastDeregisteredGroupMembers,
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    newTransitGatewayMulticastDeregisteredGroupSources,
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    newTransitGatewayMulticastDomain,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    newTransitGatewayMulticastDomainAssociation,
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    newTransitGatewayMulticastDomainAssociations,
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDomainOptions
    TransitGatewayMulticastDomainOptions (..),
    newTransitGatewayMulticastDomainOptions,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    newTransitGatewayMulticastGroup,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,

    -- * TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    newTransitGatewayMulticastRegisteredGroupMembers,
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    newTransitGatewayMulticastRegisteredGroupSources,
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,

    -- * TransitGatewayOptions
    TransitGatewayOptions (..),
    newTransitGatewayOptions,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_transitGatewayCidrBlocks,
    transitGatewayOptions_vpnEcmpSupport,

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    newTransitGatewayPeeringAttachment,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_options,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- * TransitGatewayPeeringAttachmentOptions
    TransitGatewayPeeringAttachmentOptions (..),
    newTransitGatewayPeeringAttachmentOptions,
    transitGatewayPeeringAttachmentOptions_dynamicRouting,

    -- * TransitGatewayPolicyRule
    TransitGatewayPolicyRule (..),
    newTransitGatewayPolicyRule,
    transitGatewayPolicyRule_destinationCidrBlock,
    transitGatewayPolicyRule_destinationPortRange,
    transitGatewayPolicyRule_metaData,
    transitGatewayPolicyRule_protocol,
    transitGatewayPolicyRule_sourceCidrBlock,
    transitGatewayPolicyRule_sourcePortRange,

    -- * TransitGatewayPolicyRuleMetaData
    TransitGatewayPolicyRuleMetaData (..),
    newTransitGatewayPolicyRuleMetaData,
    transitGatewayPolicyRuleMetaData_metaDataKey,
    transitGatewayPolicyRuleMetaData_metaDataValue,

    -- * TransitGatewayPolicyTable
    TransitGatewayPolicyTable (..),
    newTransitGatewayPolicyTable,
    transitGatewayPolicyTable_creationTime,
    transitGatewayPolicyTable_state,
    transitGatewayPolicyTable_tags,
    transitGatewayPolicyTable_transitGatewayId,
    transitGatewayPolicyTable_transitGatewayPolicyTableId,

    -- * TransitGatewayPolicyTableAssociation
    TransitGatewayPolicyTableAssociation (..),
    newTransitGatewayPolicyTableAssociation,
    transitGatewayPolicyTableAssociation_resourceId,
    transitGatewayPolicyTableAssociation_resourceType,
    transitGatewayPolicyTableAssociation_state,
    transitGatewayPolicyTableAssociation_transitGatewayAttachmentId,
    transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId,

    -- * TransitGatewayPolicyTableEntry
    TransitGatewayPolicyTableEntry (..),
    newTransitGatewayPolicyTableEntry,
    transitGatewayPolicyTableEntry_policyRule,
    transitGatewayPolicyTableEntry_policyRuleNumber,
    transitGatewayPolicyTableEntry_targetRouteTableId,

    -- * TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (..),
    newTransitGatewayPrefixListAttachment,
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- * TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (..),
    newTransitGatewayPrefixListReference,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_transitGatewayAttachment,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,

    -- * TransitGatewayPropagation
    TransitGatewayPropagation (..),
    newTransitGatewayPropagation,
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_state,
    transitGatewayPropagation_transitGatewayAttachmentId,
    transitGatewayPropagation_transitGatewayRouteTableAnnouncementId,
    transitGatewayPropagation_transitGatewayRouteTableId,

    -- * TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    newTransitGatewayRequestOptions,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,
    transitGatewayRequestOptions_vpnEcmpSupport,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    newTransitGatewayRoute,
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_transitGatewayRouteTableAnnouncementId,
    transitGatewayRoute_type,

    -- * TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (..),
    newTransitGatewayRouteAttachment,
    transitGatewayRouteAttachment_resourceId,
    transitGatewayRouteAttachment_resourceType,
    transitGatewayRouteAttachment_transitGatewayAttachmentId,

    -- * TransitGatewayRouteTable
    TransitGatewayRouteTable (..),
    newTransitGatewayRouteTable,
    transitGatewayRouteTable_creationTime,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_tags,
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_transitGatewayRouteTableId,

    -- * TransitGatewayRouteTableAnnouncement
    TransitGatewayRouteTableAnnouncement (..),
    newTransitGatewayRouteTableAnnouncement,
    transitGatewayRouteTableAnnouncement_announcementDirection,
    transitGatewayRouteTableAnnouncement_coreNetworkId,
    transitGatewayRouteTableAnnouncement_creationTime,
    transitGatewayRouteTableAnnouncement_peerCoreNetworkId,
    transitGatewayRouteTableAnnouncement_peerTransitGatewayId,
    transitGatewayRouteTableAnnouncement_peeringAttachmentId,
    transitGatewayRouteTableAnnouncement_state,
    transitGatewayRouteTableAnnouncement_tags,
    transitGatewayRouteTableAnnouncement_transitGatewayId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,

    -- * TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (..),
    newTransitGatewayRouteTableAssociation,
    transitGatewayRouteTableAssociation_resourceId,
    transitGatewayRouteTableAssociation_resourceType,
    transitGatewayRouteTableAssociation_state,
    transitGatewayRouteTableAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (..),
    newTransitGatewayRouteTablePropagation,
    transitGatewayRouteTablePropagation_resourceId,
    transitGatewayRouteTablePropagation_resourceType,
    transitGatewayRouteTablePropagation_state,
    transitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    transitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId,

    -- * TransitGatewayRouteTableRoute
    TransitGatewayRouteTableRoute (..),
    newTransitGatewayRouteTableRoute,
    transitGatewayRouteTableRoute_attachmentId,
    transitGatewayRouteTableRoute_destinationCidr,
    transitGatewayRouteTableRoute_prefixListId,
    transitGatewayRouteTableRoute_resourceId,
    transitGatewayRouteTableRoute_resourceType,
    transitGatewayRouteTableRoute_routeOrigin,
    transitGatewayRouteTableRoute_state,

    -- * TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (..),
    newTransitGatewayVpcAttachment,
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_vpcOwnerId,

    -- * TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (..),
    newTransitGatewayVpcAttachmentOptions,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,

    -- * TrunkInterfaceAssociation
    TrunkInterfaceAssociation (..),
    newTrunkInterfaceAssociation,
    trunkInterfaceAssociation_associationId,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_tags,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_vlanId,

    -- * TunnelOption
    TunnelOption (..),
    newTunnelOption,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_ikeVersions,
    tunnelOption_logOptions,
    tunnelOption_outsideIpAddress,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_preSharedKey,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_replayWindowSize,
    tunnelOption_startupAction,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_tunnelInsideIpv6Cidr,

    -- * UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    newUnsuccessfulInstanceCreditSpecificationItem,
    unsuccessfulInstanceCreditSpecificationItem_error,
    unsuccessfulInstanceCreditSpecificationItem_instanceId,

    -- * UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    newUnsuccessfulInstanceCreditSpecificationItemError,
    unsuccessfulInstanceCreditSpecificationItemError_code,
    unsuccessfulInstanceCreditSpecificationItemError_message,

    -- * UnsuccessfulItem
    UnsuccessfulItem (..),
    newUnsuccessfulItem,
    unsuccessfulItem_error,
    unsuccessfulItem_resourceId,

    -- * UnsuccessfulItemError
    UnsuccessfulItemError (..),
    newUnsuccessfulItemError,
    unsuccessfulItemError_code,
    unsuccessfulItemError_message,

    -- * UserBucket
    UserBucket (..),
    newUserBucket,
    userBucket_s3Bucket,
    userBucket_s3Key,

    -- * UserBucketDetails
    UserBucketDetails (..),
    newUserBucketDetails,
    userBucketDetails_s3Bucket,
    userBucketDetails_s3Key,

    -- * UserData
    UserData (..),
    newUserData,
    userData_data,

    -- * UserIdGroupPair
    UserIdGroupPair (..),
    newUserIdGroupPair,
    userIdGroupPair_description,
    userIdGroupPair_groupId,
    userIdGroupPair_groupName,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_userId,
    userIdGroupPair_vpcId,
    userIdGroupPair_vpcPeeringConnectionId,

    -- * VCpuCountRange
    VCpuCountRange (..),
    newVCpuCountRange,
    vCpuCountRange_max,
    vCpuCountRange_min,

    -- * VCpuCountRangeRequest
    VCpuCountRangeRequest (..),
    newVCpuCountRangeRequest,
    vCpuCountRangeRequest_max,
    vCpuCountRangeRequest_min,

    -- * VCpuInfo
    VCpuInfo (..),
    newVCpuInfo,
    vCpuInfo_defaultCores,
    vCpuInfo_defaultThreadsPerCore,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validCores,
    vCpuInfo_validThreadsPerCore,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_code,
    validationError_message,

    -- * ValidationWarning
    ValidationWarning (..),
    newValidationWarning,
    validationWarning_errors,

    -- * VerifiedAccessEndpoint
    VerifiedAccessEndpoint (..),
    newVerifiedAccessEndpoint,
    verifiedAccessEndpoint_applicationDomain,
    verifiedAccessEndpoint_attachmentType,
    verifiedAccessEndpoint_creationTime,
    verifiedAccessEndpoint_deletionTime,
    verifiedAccessEndpoint_description,
    verifiedAccessEndpoint_deviceValidationDomain,
    verifiedAccessEndpoint_domainCertificateArn,
    verifiedAccessEndpoint_endpointDomain,
    verifiedAccessEndpoint_endpointType,
    verifiedAccessEndpoint_lastUpdatedTime,
    verifiedAccessEndpoint_loadBalancerOptions,
    verifiedAccessEndpoint_networkInterfaceOptions,
    verifiedAccessEndpoint_securityGroupIds,
    verifiedAccessEndpoint_status,
    verifiedAccessEndpoint_tags,
    verifiedAccessEndpoint_verifiedAccessEndpointId,
    verifiedAccessEndpoint_verifiedAccessGroupId,
    verifiedAccessEndpoint_verifiedAccessInstanceId,

    -- * VerifiedAccessEndpointEniOptions
    VerifiedAccessEndpointEniOptions (..),
    newVerifiedAccessEndpointEniOptions,
    verifiedAccessEndpointEniOptions_networkInterfaceId,
    verifiedAccessEndpointEniOptions_port,
    verifiedAccessEndpointEniOptions_protocol,

    -- * VerifiedAccessEndpointLoadBalancerOptions
    VerifiedAccessEndpointLoadBalancerOptions (..),
    newVerifiedAccessEndpointLoadBalancerOptions,
    verifiedAccessEndpointLoadBalancerOptions_loadBalancerArn,
    verifiedAccessEndpointLoadBalancerOptions_port,
    verifiedAccessEndpointLoadBalancerOptions_protocol,
    verifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- * VerifiedAccessEndpointStatus
    VerifiedAccessEndpointStatus (..),
    newVerifiedAccessEndpointStatus,
    verifiedAccessEndpointStatus_code,
    verifiedAccessEndpointStatus_message,

    -- * VerifiedAccessGroup
    VerifiedAccessGroup (..),
    newVerifiedAccessGroup,
    verifiedAccessGroup_creationTime,
    verifiedAccessGroup_deletionTime,
    verifiedAccessGroup_description,
    verifiedAccessGroup_lastUpdatedTime,
    verifiedAccessGroup_owner,
    verifiedAccessGroup_tags,
    verifiedAccessGroup_verifiedAccessGroupArn,
    verifiedAccessGroup_verifiedAccessGroupId,
    verifiedAccessGroup_verifiedAccessInstanceId,

    -- * VerifiedAccessInstance
    VerifiedAccessInstance (..),
    newVerifiedAccessInstance,
    verifiedAccessInstance_creationTime,
    verifiedAccessInstance_description,
    verifiedAccessInstance_lastUpdatedTime,
    verifiedAccessInstance_tags,
    verifiedAccessInstance_verifiedAccessInstanceId,
    verifiedAccessInstance_verifiedAccessTrustProviders,

    -- * VerifiedAccessInstanceLoggingConfiguration
    VerifiedAccessInstanceLoggingConfiguration (..),
    newVerifiedAccessInstanceLoggingConfiguration,
    verifiedAccessInstanceLoggingConfiguration_accessLogs,
    verifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId,

    -- * VerifiedAccessLogCloudWatchLogsDestination
    VerifiedAccessLogCloudWatchLogsDestination (..),
    newVerifiedAccessLogCloudWatchLogsDestination,
    verifiedAccessLogCloudWatchLogsDestination_deliveryStatus,
    verifiedAccessLogCloudWatchLogsDestination_enabled,
    verifiedAccessLogCloudWatchLogsDestination_logGroup,

    -- * VerifiedAccessLogCloudWatchLogsDestinationOptions
    VerifiedAccessLogCloudWatchLogsDestinationOptions (..),
    newVerifiedAccessLogCloudWatchLogsDestinationOptions,
    verifiedAccessLogCloudWatchLogsDestinationOptions_logGroup,
    verifiedAccessLogCloudWatchLogsDestinationOptions_enabled,

    -- * VerifiedAccessLogDeliveryStatus
    VerifiedAccessLogDeliveryStatus (..),
    newVerifiedAccessLogDeliveryStatus,
    verifiedAccessLogDeliveryStatus_code,
    verifiedAccessLogDeliveryStatus_message,

    -- * VerifiedAccessLogKinesisDataFirehoseDestination
    VerifiedAccessLogKinesisDataFirehoseDestination (..),
    newVerifiedAccessLogKinesisDataFirehoseDestination,
    verifiedAccessLogKinesisDataFirehoseDestination_deliveryStatus,
    verifiedAccessLogKinesisDataFirehoseDestination_deliveryStream,
    verifiedAccessLogKinesisDataFirehoseDestination_enabled,

    -- * VerifiedAccessLogKinesisDataFirehoseDestinationOptions
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions (..),
    newVerifiedAccessLogKinesisDataFirehoseDestinationOptions,
    verifiedAccessLogKinesisDataFirehoseDestinationOptions_deliveryStream,
    verifiedAccessLogKinesisDataFirehoseDestinationOptions_enabled,

    -- * VerifiedAccessLogOptions
    VerifiedAccessLogOptions (..),
    newVerifiedAccessLogOptions,
    verifiedAccessLogOptions_cloudWatchLogs,
    verifiedAccessLogOptions_kinesisDataFirehose,
    verifiedAccessLogOptions_s3,

    -- * VerifiedAccessLogS3Destination
    VerifiedAccessLogS3Destination (..),
    newVerifiedAccessLogS3Destination,
    verifiedAccessLogS3Destination_bucketName,
    verifiedAccessLogS3Destination_bucketOwner,
    verifiedAccessLogS3Destination_deliveryStatus,
    verifiedAccessLogS3Destination_enabled,
    verifiedAccessLogS3Destination_prefix,

    -- * VerifiedAccessLogS3DestinationOptions
    VerifiedAccessLogS3DestinationOptions (..),
    newVerifiedAccessLogS3DestinationOptions,
    verifiedAccessLogS3DestinationOptions_bucketName,
    verifiedAccessLogS3DestinationOptions_bucketOwner,
    verifiedAccessLogS3DestinationOptions_prefix,
    verifiedAccessLogS3DestinationOptions_enabled,

    -- * VerifiedAccessLogs
    VerifiedAccessLogs (..),
    newVerifiedAccessLogs,
    verifiedAccessLogs_cloudWatchLogs,
    verifiedAccessLogs_kinesisDataFirehose,
    verifiedAccessLogs_s3,

    -- * VerifiedAccessTrustProvider
    VerifiedAccessTrustProvider (..),
    newVerifiedAccessTrustProvider,
    verifiedAccessTrustProvider_creationTime,
    verifiedAccessTrustProvider_description,
    verifiedAccessTrustProvider_deviceOptions,
    verifiedAccessTrustProvider_deviceTrustProviderType,
    verifiedAccessTrustProvider_lastUpdatedTime,
    verifiedAccessTrustProvider_oidcOptions,
    verifiedAccessTrustProvider_policyReferenceName,
    verifiedAccessTrustProvider_tags,
    verifiedAccessTrustProvider_trustProviderType,
    verifiedAccessTrustProvider_userTrustProviderType,
    verifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- * VerifiedAccessTrustProviderCondensed
    VerifiedAccessTrustProviderCondensed (..),
    newVerifiedAccessTrustProviderCondensed,
    verifiedAccessTrustProviderCondensed_description,
    verifiedAccessTrustProviderCondensed_deviceTrustProviderType,
    verifiedAccessTrustProviderCondensed_trustProviderType,
    verifiedAccessTrustProviderCondensed_userTrustProviderType,
    verifiedAccessTrustProviderCondensed_verifiedAccessTrustProviderId,

    -- * VgwTelemetry
    VgwTelemetry (..),
    newVgwTelemetry,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_certificateArn,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_outsideIpAddress,
    vgwTelemetry_status,
    vgwTelemetry_statusMessage,

    -- * Volume
    Volume (..),
    newVolume,
    volume_attachments,
    volume_fastRestored,
    volume_iops,
    volume_kmsKeyId,
    volume_multiAttachEnabled,
    volume_outpostArn,
    volume_tags,
    volume_throughput,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- * VolumeAttachment
    VolumeAttachment (..),
    newVolumeAttachment,
    volumeAttachment_attachTime,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_instanceId,
    volumeAttachment_state,
    volumeAttachment_volumeId,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_size,

    -- * VolumeModification
    VolumeModification (..),
    newVolumeModification,
    volumeModification_endTime,
    volumeModification_modificationState,
    volumeModification_originalIops,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_originalSize,
    volumeModification_originalThroughput,
    volumeModification_originalVolumeType,
    volumeModification_progress,
    volumeModification_startTime,
    volumeModification_statusMessage,
    volumeModification_targetIops,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_targetSize,
    volumeModification_targetThroughput,
    volumeModification_targetVolumeType,
    volumeModification_volumeId,

    -- * VolumeStatusAction
    VolumeStatusAction (..),
    newVolumeStatusAction,
    volumeStatusAction_code,
    volumeStatusAction_description,
    volumeStatusAction_eventId,
    volumeStatusAction_eventType,

    -- * VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    newVolumeStatusAttachmentStatus,
    volumeStatusAttachmentStatus_instanceId,
    volumeStatusAttachmentStatus_ioPerformance,

    -- * VolumeStatusDetails
    VolumeStatusDetails (..),
    newVolumeStatusDetails,
    volumeStatusDetails_name,
    volumeStatusDetails_status,

    -- * VolumeStatusEvent
    VolumeStatusEvent (..),
    newVolumeStatusEvent,
    volumeStatusEvent_description,
    volumeStatusEvent_eventId,
    volumeStatusEvent_eventType,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_notBefore,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    newVolumeStatusInfo,
    volumeStatusInfo_details,
    volumeStatusInfo_status,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    newVolumeStatusItem,
    volumeStatusItem_actions,
    volumeStatusItem_attachmentStatuses,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_events,
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeId,
    volumeStatusItem_volumeStatus,

    -- * Vpc
    Vpc (..),
    newVpc,
    vpc_cidrBlockAssociationSet,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_isDefault,
    vpc_ownerId,
    vpc_tags,
    vpc_cidrBlock,
    vpc_dhcpOptionsId,
    vpc_instanceTenancy,
    vpc_state,
    vpc_vpcId,

    -- * VpcAttachment
    VpcAttachment (..),
    newVpcAttachment,
    vpcAttachment_state,
    vpcAttachment_vpcId,

    -- * VpcCidrBlockAssociation
    VpcCidrBlockAssociation (..),
    newVpcCidrBlockAssociation,
    vpcCidrBlockAssociation_associationId,
    vpcCidrBlockAssociation_cidrBlock,
    vpcCidrBlockAssociation_cidrBlockState,

    -- * VpcCidrBlockState
    VpcCidrBlockState (..),
    newVpcCidrBlockState,
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- * VpcClassicLink
    VpcClassicLink (..),
    newVpcClassicLink,
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_tags,
    vpcClassicLink_vpcId,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_dnsOptions,
    vpcEndpoint_groups,
    vpcEndpoint_ipAddressType,
    vpcEndpoint_lastError,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_ownerId,
    vpcEndpoint_policyDocument,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_serviceName,
    vpcEndpoint_state,
    vpcEndpoint_subnetIds,
    vpcEndpoint_tags,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_vpcId,

    -- * VpcEndpointConnection
    VpcEndpointConnection (..),
    newVpcEndpointConnection,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_ipAddressType,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_tags,
    vpcEndpointConnection_vpcEndpointConnectionId,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_vpcEndpointState,

    -- * VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (..),
    newVpcIpv6CidrBlockAssociation,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_status,
    vpcPeeringConnection_tags,
    vpcPeeringConnection_vpcPeeringConnectionId,

    -- * VpcPeeringConnectionOptionsDescription
    VpcPeeringConnectionOptionsDescription (..),
    newVpcPeeringConnectionOptionsDescription,
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,

    -- * VpcPeeringConnectionStateReason
    VpcPeeringConnectionStateReason (..),
    newVpcPeeringConnectionStateReason,
    vpcPeeringConnectionStateReason_code,
    vpcPeeringConnectionStateReason_message,

    -- * VpcPeeringConnectionVpcInfo
    VpcPeeringConnectionVpcInfo (..),
    newVpcPeeringConnectionVpcInfo,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_vpcId,

    -- * VpnConnection
    VpnConnection (..),
    newVpnConnection,
    vpnConnection_category,
    vpnConnection_coreNetworkArn,
    vpnConnection_coreNetworkAttachmentArn,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_gatewayAssociationState,
    vpnConnection_options,
    vpnConnection_routes,
    vpnConnection_tags,
    vpnConnection_transitGatewayId,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnGatewayId,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- * VpnConnectionDeviceType
    VpnConnectionDeviceType (..),
    newVpnConnectionDeviceType,
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_software,
    vpnConnectionDeviceType_vendor,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,

    -- * VpnConnectionOptions
    VpnConnectionOptions (..),
    newVpnConnectionOptions,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_outsideIpAddressType,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_transportTransitGatewayAttachmentId,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_tunnelOptions,

    -- * VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (..),
    newVpnConnectionOptionsSpecification,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_outsideIpAddressType,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_tunnelOptions,

    -- * VpnGateway
    VpnGateway (..),
    newVpnGateway,
    vpnGateway_amazonSideAsn,
    vpnGateway_availabilityZone,
    vpnGateway_state,
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_vpcAttachments,
    vpnGateway_vpnGatewayId,

    -- * VpnStaticRoute
    VpnStaticRoute (..),
    newVpnStaticRoute,
    vpnStaticRoute_destinationCidrBlock,
    vpnStaticRoute_source,
    vpnStaticRoute_state,

    -- * VpnTunnelLogOptions
    VpnTunnelLogOptions (..),
    newVpnTunnelLogOptions,
    vpnTunnelLogOptions_cloudWatchLogOptions,

    -- * VpnTunnelLogOptionsSpecification
    VpnTunnelLogOptionsSpecification (..),
    newVpnTunnelLogOptionsSpecification,
    vpnTunnelLogOptionsSpecification_cloudWatchLogOptions,

    -- * VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (..),
    newVpnTunnelOptionsSpecification,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_logOptions,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AcceleratorCount
import Amazonka.EC2.Types.AcceleratorCountRequest
import Amazonka.EC2.Types.AcceleratorManufacturer
import Amazonka.EC2.Types.AcceleratorName
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiB
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiBRequest
import Amazonka.EC2.Types.AcceleratorType
import Amazonka.EC2.Types.AccessScopeAnalysisFinding
import Amazonka.EC2.Types.AccessScopePath
import Amazonka.EC2.Types.AccessScopePathRequest
import Amazonka.EC2.Types.AccountAttribute
import Amazonka.EC2.Types.AccountAttributeName
import Amazonka.EC2.Types.AccountAttributeValue
import Amazonka.EC2.Types.ActiveInstance
import Amazonka.EC2.Types.ActivityStatus
import Amazonka.EC2.Types.AddIpamOperatingRegion
import Amazonka.EC2.Types.AddPrefixListEntry
import Amazonka.EC2.Types.AddedPrincipal
import Amazonka.EC2.Types.AdditionalDetail
import Amazonka.EC2.Types.Address
import Amazonka.EC2.Types.AddressAttribute
import Amazonka.EC2.Types.AddressAttributeName
import Amazonka.EC2.Types.AddressFamily
import Amazonka.EC2.Types.AddressStatus
import Amazonka.EC2.Types.AddressTransfer
import Amazonka.EC2.Types.AddressTransferStatus
import Amazonka.EC2.Types.Affinity
import Amazonka.EC2.Types.AllocationState
import Amazonka.EC2.Types.AllocationStrategy
import Amazonka.EC2.Types.AllocationType
import Amazonka.EC2.Types.AllowedPrincipal
import Amazonka.EC2.Types.AllowsMultipleInstanceTypes
import Amazonka.EC2.Types.AlternatePathHint
import Amazonka.EC2.Types.AnalysisAclRule
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.AnalysisLoadBalancerListener
import Amazonka.EC2.Types.AnalysisLoadBalancerTarget
import Amazonka.EC2.Types.AnalysisPacketHeader
import Amazonka.EC2.Types.AnalysisRouteTableRoute
import Amazonka.EC2.Types.AnalysisSecurityGroupRule
import Amazonka.EC2.Types.AnalysisStatus
import Amazonka.EC2.Types.ApplianceModeSupportValue
import Amazonka.EC2.Types.ArchitectureType
import Amazonka.EC2.Types.ArchitectureValues
import Amazonka.EC2.Types.AssignedPrivateIpAddress
import Amazonka.EC2.Types.AssociatedNetworkType
import Amazonka.EC2.Types.AssociatedRole
import Amazonka.EC2.Types.AssociatedTargetNetwork
import Amazonka.EC2.Types.AssociationStatus
import Amazonka.EC2.Types.AssociationStatusCode
import Amazonka.EC2.Types.AthenaIntegration
import Amazonka.EC2.Types.AttachmentEnaSrdSpecification
import Amazonka.EC2.Types.AttachmentEnaSrdUdpSpecification
import Amazonka.EC2.Types.AttachmentStatus
import Amazonka.EC2.Types.AttributeBooleanValue
import Amazonka.EC2.Types.AttributeValue
import Amazonka.EC2.Types.AuthorizationRule
import Amazonka.EC2.Types.AutoAcceptSharedAssociationsValue
import Amazonka.EC2.Types.AutoAcceptSharedAttachmentsValue
import Amazonka.EC2.Types.AutoPlacement
import Amazonka.EC2.Types.AvailabilityZone
import Amazonka.EC2.Types.AvailabilityZoneMessage
import Amazonka.EC2.Types.AvailabilityZoneOptInStatus
import Amazonka.EC2.Types.AvailabilityZoneState
import Amazonka.EC2.Types.AvailableCapacity
import Amazonka.EC2.Types.BareMetal
import Amazonka.EC2.Types.BaselineEbsBandwidthMbps
import Amazonka.EC2.Types.BaselineEbsBandwidthMbpsRequest
import Amazonka.EC2.Types.BatchState
import Amazonka.EC2.Types.BgpStatus
import Amazonka.EC2.Types.BlobAttributeValue
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.BootModeType
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.BundleTask
import Amazonka.EC2.Types.BundleTaskError
import Amazonka.EC2.Types.BundleTaskState
import Amazonka.EC2.Types.BurstablePerformance
import Amazonka.EC2.Types.ByoipCidr
import Amazonka.EC2.Types.ByoipCidrState
import Amazonka.EC2.Types.CancelBatchErrorCode
import Amazonka.EC2.Types.CancelCapacityReservationFleetError
import Amazonka.EC2.Types.CancelSpotFleetRequestsError
import Amazonka.EC2.Types.CancelSpotFleetRequestsErrorItem
import Amazonka.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Amazonka.EC2.Types.CancelSpotInstanceRequestState
import Amazonka.EC2.Types.CancelledSpotInstanceRequest
import Amazonka.EC2.Types.CapacityAllocation
import Amazonka.EC2.Types.CapacityReservation
import Amazonka.EC2.Types.CapacityReservationFleet
import Amazonka.EC2.Types.CapacityReservationFleetCancellationState
import Amazonka.EC2.Types.CapacityReservationFleetState
import Amazonka.EC2.Types.CapacityReservationGroup
import Amazonka.EC2.Types.CapacityReservationInstancePlatform
import Amazonka.EC2.Types.CapacityReservationOptions
import Amazonka.EC2.Types.CapacityReservationOptionsRequest
import Amazonka.EC2.Types.CapacityReservationPreference
import Amazonka.EC2.Types.CapacityReservationSpecification
import Amazonka.EC2.Types.CapacityReservationSpecificationResponse
import Amazonka.EC2.Types.CapacityReservationState
import Amazonka.EC2.Types.CapacityReservationTarget
import Amazonka.EC2.Types.CapacityReservationTargetResponse
import Amazonka.EC2.Types.CapacityReservationTenancy
import Amazonka.EC2.Types.CarrierGateway
import Amazonka.EC2.Types.CarrierGatewayState
import Amazonka.EC2.Types.CertificateAuthentication
import Amazonka.EC2.Types.CertificateAuthenticationRequest
import Amazonka.EC2.Types.CidrAuthorizationContext
import Amazonka.EC2.Types.CidrBlock
import Amazonka.EC2.Types.ClassicLinkDnsSupport
import Amazonka.EC2.Types.ClassicLinkInstance
import Amazonka.EC2.Types.ClassicLoadBalancer
import Amazonka.EC2.Types.ClassicLoadBalancersConfig
import Amazonka.EC2.Types.ClientCertificateRevocationListStatus
import Amazonka.EC2.Types.ClientCertificateRevocationListStatusCode
import Amazonka.EC2.Types.ClientConnectOptions
import Amazonka.EC2.Types.ClientConnectResponseOptions
import Amazonka.EC2.Types.ClientData
import Amazonka.EC2.Types.ClientLoginBannerOptions
import Amazonka.EC2.Types.ClientLoginBannerResponseOptions
import Amazonka.EC2.Types.ClientVpnAuthentication
import Amazonka.EC2.Types.ClientVpnAuthenticationRequest
import Amazonka.EC2.Types.ClientVpnAuthenticationType
import Amazonka.EC2.Types.ClientVpnAuthorizationRuleStatus
import Amazonka.EC2.Types.ClientVpnAuthorizationRuleStatusCode
import Amazonka.EC2.Types.ClientVpnConnection
import Amazonka.EC2.Types.ClientVpnConnectionStatus
import Amazonka.EC2.Types.ClientVpnConnectionStatusCode
import Amazonka.EC2.Types.ClientVpnEndpoint
import Amazonka.EC2.Types.ClientVpnEndpointAttributeStatus
import Amazonka.EC2.Types.ClientVpnEndpointAttributeStatusCode
import Amazonka.EC2.Types.ClientVpnEndpointStatus
import Amazonka.EC2.Types.ClientVpnEndpointStatusCode
import Amazonka.EC2.Types.ClientVpnRoute
import Amazonka.EC2.Types.ClientVpnRouteStatus
import Amazonka.EC2.Types.ClientVpnRouteStatusCode
import Amazonka.EC2.Types.CloudWatchLogOptions
import Amazonka.EC2.Types.CloudWatchLogOptionsSpecification
import Amazonka.EC2.Types.CoipAddressUsage
import Amazonka.EC2.Types.CoipCidr
import Amazonka.EC2.Types.CoipPool
import Amazonka.EC2.Types.ConnectionLogOptions
import Amazonka.EC2.Types.ConnectionLogResponseOptions
import Amazonka.EC2.Types.ConnectionNotification
import Amazonka.EC2.Types.ConnectionNotificationState
import Amazonka.EC2.Types.ConnectionNotificationType
import Amazonka.EC2.Types.ConnectivityType
import Amazonka.EC2.Types.ContainerFormat
import Amazonka.EC2.Types.ConversionTask
import Amazonka.EC2.Types.ConversionTaskState
import Amazonka.EC2.Types.CopyTagsFromSource
import Amazonka.EC2.Types.CpuManufacturer
import Amazonka.EC2.Types.CpuOptions
import Amazonka.EC2.Types.CpuOptionsRequest
import Amazonka.EC2.Types.CreateFleetError
import Amazonka.EC2.Types.CreateFleetInstance
import Amazonka.EC2.Types.CreateTransitGatewayConnectRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayMulticastDomainRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayPeeringAttachmentRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
import Amazonka.EC2.Types.CreateVerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.CreateVerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderDeviceOptions
import Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderOidcOptions
import Amazonka.EC2.Types.CreateVolumePermission
import Amazonka.EC2.Types.CreateVolumePermissionModifications
import Amazonka.EC2.Types.CreditSpecification
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.CustomerGateway
import Amazonka.EC2.Types.DataQuery
import Amazonka.EC2.Types.DataResponse
import Amazonka.EC2.Types.DatafeedSubscriptionState
import Amazonka.EC2.Types.DefaultRouteTableAssociationValue
import Amazonka.EC2.Types.DefaultRouteTablePropagationValue
import Amazonka.EC2.Types.DefaultTargetCapacityType
import Amazonka.EC2.Types.DeleteFleetError
import Amazonka.EC2.Types.DeleteFleetErrorCode
import Amazonka.EC2.Types.DeleteFleetErrorItem
import Amazonka.EC2.Types.DeleteFleetSuccessItem
import Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
import Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
import Amazonka.EC2.Types.DeleteQueuedReservedInstancesError
import Amazonka.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import Amazonka.EC2.Types.DeregisterInstanceTagAttributeRequest
import Amazonka.EC2.Types.DescribeFastLaunchImagesSuccessItem
import Amazonka.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.DescribeFleetError
import Amazonka.EC2.Types.DescribeFleetsInstances
import Amazonka.EC2.Types.DestinationFileFormat
import Amazonka.EC2.Types.DestinationOptionsRequest
import Amazonka.EC2.Types.DestinationOptionsResponse
import Amazonka.EC2.Types.DeviceOptions
import Amazonka.EC2.Types.DeviceTrustProviderType
import Amazonka.EC2.Types.DeviceType
import Amazonka.EC2.Types.DhcpConfiguration
import Amazonka.EC2.Types.DhcpOptions
import Amazonka.EC2.Types.DirectoryServiceAuthentication
import Amazonka.EC2.Types.DirectoryServiceAuthenticationRequest
import Amazonka.EC2.Types.DisableFastSnapshotRestoreErrorItem
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateError
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import Amazonka.EC2.Types.DisableFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.DiskImage
import Amazonka.EC2.Types.DiskImageDescription
import Amazonka.EC2.Types.DiskImageDetail
import Amazonka.EC2.Types.DiskImageFormat
import Amazonka.EC2.Types.DiskImageVolumeDescription
import Amazonka.EC2.Types.DiskInfo
import Amazonka.EC2.Types.DiskType
import Amazonka.EC2.Types.DnsEntry
import Amazonka.EC2.Types.DnsNameState
import Amazonka.EC2.Types.DnsOptions
import Amazonka.EC2.Types.DnsOptionsSpecification
import Amazonka.EC2.Types.DnsRecordIpType
import Amazonka.EC2.Types.DnsServersOptionsModifyStructure
import Amazonka.EC2.Types.DnsSupportValue
import Amazonka.EC2.Types.DomainType
import Amazonka.EC2.Types.DynamicRoutingValue
import Amazonka.EC2.Types.EbsBlockDevice
import Amazonka.EC2.Types.EbsEncryptionSupport
import Amazonka.EC2.Types.EbsInfo
import Amazonka.EC2.Types.EbsInstanceBlockDevice
import Amazonka.EC2.Types.EbsInstanceBlockDeviceSpecification
import Amazonka.EC2.Types.EbsNvmeSupport
import Amazonka.EC2.Types.EbsOptimizedInfo
import Amazonka.EC2.Types.EbsOptimizedSupport
import Amazonka.EC2.Types.EfaInfo
import Amazonka.EC2.Types.EgressOnlyInternetGateway
import Amazonka.EC2.Types.ElasticGpuAssociation
import Amazonka.EC2.Types.ElasticGpuHealth
import Amazonka.EC2.Types.ElasticGpuSpecification
import Amazonka.EC2.Types.ElasticGpuSpecificationResponse
import Amazonka.EC2.Types.ElasticGpuState
import Amazonka.EC2.Types.ElasticGpuStatus
import Amazonka.EC2.Types.ElasticGpus
import Amazonka.EC2.Types.ElasticInferenceAccelerator
import Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation
import Amazonka.EC2.Types.EnaSrdSpecification
import Amazonka.EC2.Types.EnaSrdUdpSpecification
import Amazonka.EC2.Types.EnaSupport
import Amazonka.EC2.Types.EnableFastSnapshotRestoreErrorItem
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateError
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import Amazonka.EC2.Types.EnableFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.EnclaveOptions
import Amazonka.EC2.Types.EnclaveOptionsRequest
import Amazonka.EC2.Types.EndDateType
import Amazonka.EC2.Types.EphemeralNvmeSupport
import Amazonka.EC2.Types.EventCode
import Amazonka.EC2.Types.EventInformation
import Amazonka.EC2.Types.EventType
import Amazonka.EC2.Types.ExcessCapacityTerminationPolicy
import Amazonka.EC2.Types.Explanation
import Amazonka.EC2.Types.ExportEnvironment
import Amazonka.EC2.Types.ExportImageTask
import Amazonka.EC2.Types.ExportTask
import Amazonka.EC2.Types.ExportTaskS3Location
import Amazonka.EC2.Types.ExportTaskS3LocationRequest
import Amazonka.EC2.Types.ExportTaskState
import Amazonka.EC2.Types.ExportToS3Task
import Amazonka.EC2.Types.ExportToS3TaskSpecification
import Amazonka.EC2.Types.FailedCapacityReservationFleetCancellationResult
import Amazonka.EC2.Types.FailedQueuedPurchaseDeletion
import Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationRequest
import Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationResponse
import Amazonka.EC2.Types.FastLaunchResourceType
import Amazonka.EC2.Types.FastLaunchSnapshotConfigurationRequest
import Amazonka.EC2.Types.FastLaunchSnapshotConfigurationResponse
import Amazonka.EC2.Types.FastLaunchStateCode
import Amazonka.EC2.Types.FastSnapshotRestoreStateCode
import Amazonka.EC2.Types.FederatedAuthentication
import Amazonka.EC2.Types.FederatedAuthenticationRequest
import Amazonka.EC2.Types.Filter
import Amazonka.EC2.Types.FindingsFound
import Amazonka.EC2.Types.FleetActivityStatus
import Amazonka.EC2.Types.FleetCapacityReservation
import Amazonka.EC2.Types.FleetCapacityReservationTenancy
import Amazonka.EC2.Types.FleetCapacityReservationUsageStrategy
import Amazonka.EC2.Types.FleetData
import Amazonka.EC2.Types.FleetEventType
import Amazonka.EC2.Types.FleetExcessCapacityTerminationPolicy
import Amazonka.EC2.Types.FleetInstanceMatchCriteria
import Amazonka.EC2.Types.FleetLaunchTemplateConfig
import Amazonka.EC2.Types.FleetLaunchTemplateConfigRequest
import Amazonka.EC2.Types.FleetLaunchTemplateOverrides
import Amazonka.EC2.Types.FleetLaunchTemplateOverridesRequest
import Amazonka.EC2.Types.FleetLaunchTemplateSpecification
import Amazonka.EC2.Types.FleetLaunchTemplateSpecificationRequest
import Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
import Amazonka.EC2.Types.FleetReplacementStrategy
import Amazonka.EC2.Types.FleetSpotCapacityRebalance
import Amazonka.EC2.Types.FleetSpotCapacityRebalanceRequest
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategies
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Amazonka.EC2.Types.FleetStateCode
import Amazonka.EC2.Types.FleetType
import Amazonka.EC2.Types.FlowLog
import Amazonka.EC2.Types.FlowLogsResourceType
import Amazonka.EC2.Types.FpgaDeviceInfo
import Amazonka.EC2.Types.FpgaDeviceMemoryInfo
import Amazonka.EC2.Types.FpgaImage
import Amazonka.EC2.Types.FpgaImageAttribute
import Amazonka.EC2.Types.FpgaImageAttributeName
import Amazonka.EC2.Types.FpgaImageState
import Amazonka.EC2.Types.FpgaImageStateCode
import Amazonka.EC2.Types.FpgaInfo
import Amazonka.EC2.Types.GatewayAssociationState
import Amazonka.EC2.Types.GatewayType
import Amazonka.EC2.Types.GpuDeviceInfo
import Amazonka.EC2.Types.GpuDeviceMemoryInfo
import Amazonka.EC2.Types.GpuInfo
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.HibernationOptions
import Amazonka.EC2.Types.HibernationOptionsRequest
import Amazonka.EC2.Types.HistoryRecord
import Amazonka.EC2.Types.HistoryRecordEntry
import Amazonka.EC2.Types.Host
import Amazonka.EC2.Types.HostInstance
import Amazonka.EC2.Types.HostOffering
import Amazonka.EC2.Types.HostProperties
import Amazonka.EC2.Types.HostRecovery
import Amazonka.EC2.Types.HostReservation
import Amazonka.EC2.Types.HostTenancy
import Amazonka.EC2.Types.HostnameType
import Amazonka.EC2.Types.HttpTokensState
import Amazonka.EC2.Types.HypervisorType
import Amazonka.EC2.Types.IKEVersionsListValue
import Amazonka.EC2.Types.IKEVersionsRequestListValue
import Amazonka.EC2.Types.IamInstanceProfile
import Amazonka.EC2.Types.IamInstanceProfileAssociation
import Amazonka.EC2.Types.IamInstanceProfileAssociationState
import Amazonka.EC2.Types.IamInstanceProfileSpecification
import Amazonka.EC2.Types.IcmpTypeCode
import Amazonka.EC2.Types.IdFormat
import Amazonka.EC2.Types.Igmpv2SupportValue
import Amazonka.EC2.Types.Image
import Amazonka.EC2.Types.ImageAttributeName
import Amazonka.EC2.Types.ImageDiskContainer
import Amazonka.EC2.Types.ImageRecycleBinInfo
import Amazonka.EC2.Types.ImageState
import Amazonka.EC2.Types.ImageTypeValues
import Amazonka.EC2.Types.ImdsSupportValues
import Amazonka.EC2.Types.ImportImageLicenseConfigurationRequest
import Amazonka.EC2.Types.ImportImageLicenseConfigurationResponse
import Amazonka.EC2.Types.ImportImageTask
import Amazonka.EC2.Types.ImportInstanceLaunchSpecification
import Amazonka.EC2.Types.ImportInstanceTaskDetails
import Amazonka.EC2.Types.ImportInstanceVolumeDetailItem
import Amazonka.EC2.Types.ImportSnapshotTask
import Amazonka.EC2.Types.ImportVolumeTaskDetails
import Amazonka.EC2.Types.InferenceAcceleratorInfo
import Amazonka.EC2.Types.InferenceDeviceInfo
import Amazonka.EC2.Types.Instance
import Amazonka.EC2.Types.InstanceAttributeName
import Amazonka.EC2.Types.InstanceAutoRecoveryState
import Amazonka.EC2.Types.InstanceBlockDeviceMapping
import Amazonka.EC2.Types.InstanceBlockDeviceMappingSpecification
import Amazonka.EC2.Types.InstanceCapacity
import Amazonka.EC2.Types.InstanceCount
import Amazonka.EC2.Types.InstanceCreditSpecification
import Amazonka.EC2.Types.InstanceCreditSpecificationRequest
import Amazonka.EC2.Types.InstanceEventWindow
import Amazonka.EC2.Types.InstanceEventWindowAssociationRequest
import Amazonka.EC2.Types.InstanceEventWindowAssociationTarget
import Amazonka.EC2.Types.InstanceEventWindowDisassociationRequest
import Amazonka.EC2.Types.InstanceEventWindowState
import Amazonka.EC2.Types.InstanceEventWindowStateChange
import Amazonka.EC2.Types.InstanceEventWindowTimeRange
import Amazonka.EC2.Types.InstanceEventWindowTimeRangeRequest
import Amazonka.EC2.Types.InstanceExportDetails
import Amazonka.EC2.Types.InstanceFamilyCreditSpecification
import Amazonka.EC2.Types.InstanceGeneration
import Amazonka.EC2.Types.InstanceHealthStatus
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.InstanceIpv4Prefix
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.InstanceIpv6AddressRequest
import Amazonka.EC2.Types.InstanceIpv6Prefix
import Amazonka.EC2.Types.InstanceLifecycle
import Amazonka.EC2.Types.InstanceLifecycleType
import Amazonka.EC2.Types.InstanceMaintenanceOptions
import Amazonka.EC2.Types.InstanceMaintenanceOptionsRequest
import Amazonka.EC2.Types.InstanceMarketOptionsRequest
import Amazonka.EC2.Types.InstanceMatchCriteria
import Amazonka.EC2.Types.InstanceMetadataEndpointState
import Amazonka.EC2.Types.InstanceMetadataOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsResponse
import Amazonka.EC2.Types.InstanceMetadataOptionsState
import Amazonka.EC2.Types.InstanceMetadataProtocolState
import Amazonka.EC2.Types.InstanceMetadataTagsState
import Amazonka.EC2.Types.InstanceMonitoring
import Amazonka.EC2.Types.InstanceNetworkInterface
import Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
import Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstancePrivateIpAddress
import Amazonka.EC2.Types.InstanceRequirements
import Amazonka.EC2.Types.InstanceRequirementsRequest
import Amazonka.EC2.Types.InstanceRequirementsWithMetadataRequest
import Amazonka.EC2.Types.InstanceSpecification
import Amazonka.EC2.Types.InstanceState
import Amazonka.EC2.Types.InstanceStateChange
import Amazonka.EC2.Types.InstanceStateName
import Amazonka.EC2.Types.InstanceStatus
import Amazonka.EC2.Types.InstanceStatusDetails
import Amazonka.EC2.Types.InstanceStatusEvent
import Amazonka.EC2.Types.InstanceStatusSummary
import Amazonka.EC2.Types.InstanceStorageEncryptionSupport
import Amazonka.EC2.Types.InstanceStorageInfo
import Amazonka.EC2.Types.InstanceTagNotificationAttribute
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.InstanceTypeHypervisor
import Amazonka.EC2.Types.InstanceTypeInfo
import Amazonka.EC2.Types.InstanceTypeInfoFromInstanceRequirements
import Amazonka.EC2.Types.InstanceTypeOffering
import Amazonka.EC2.Types.InstanceUsage
import Amazonka.EC2.Types.IntegrateServices
import Amazonka.EC2.Types.InterfacePermissionType
import Amazonka.EC2.Types.InterfaceProtocolType
import Amazonka.EC2.Types.InternetGateway
import Amazonka.EC2.Types.InternetGatewayAttachment
import Amazonka.EC2.Types.IpAddressType
import Amazonka.EC2.Types.IpPermission
import Amazonka.EC2.Types.IpRange
import Amazonka.EC2.Types.Ipam
import Amazonka.EC2.Types.IpamAddressHistoryRecord
import Amazonka.EC2.Types.IpamAddressHistoryResourceType
import Amazonka.EC2.Types.IpamCidrAuthorizationContext
import Amazonka.EC2.Types.IpamComplianceStatus
import Amazonka.EC2.Types.IpamManagementState
import Amazonka.EC2.Types.IpamOperatingRegion
import Amazonka.EC2.Types.IpamOverlapStatus
import Amazonka.EC2.Types.IpamPool
import Amazonka.EC2.Types.IpamPoolAllocation
import Amazonka.EC2.Types.IpamPoolAllocationResourceType
import Amazonka.EC2.Types.IpamPoolAwsService
import Amazonka.EC2.Types.IpamPoolCidr
import Amazonka.EC2.Types.IpamPoolCidrFailureCode
import Amazonka.EC2.Types.IpamPoolCidrFailureReason
import Amazonka.EC2.Types.IpamPoolCidrState
import Amazonka.EC2.Types.IpamPoolState
import Amazonka.EC2.Types.IpamResourceCidr
import Amazonka.EC2.Types.IpamResourceTag
import Amazonka.EC2.Types.IpamResourceType
import Amazonka.EC2.Types.IpamScope
import Amazonka.EC2.Types.IpamScopeState
import Amazonka.EC2.Types.IpamScopeType
import Amazonka.EC2.Types.IpamState
import Amazonka.EC2.Types.Ipv4PrefixSpecification
import Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv4PrefixSpecificationResponse
import Amazonka.EC2.Types.Ipv6CidrAssociation
import Amazonka.EC2.Types.Ipv6CidrBlock
import Amazonka.EC2.Types.Ipv6Pool
import Amazonka.EC2.Types.Ipv6PrefixSpecification
import Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv6PrefixSpecificationResponse
import Amazonka.EC2.Types.Ipv6Range
import Amazonka.EC2.Types.Ipv6SupportValue
import Amazonka.EC2.Types.KeyFormat
import Amazonka.EC2.Types.KeyPairInfo
import Amazonka.EC2.Types.KeyType
import Amazonka.EC2.Types.LastError
import Amazonka.EC2.Types.LaunchPermission
import Amazonka.EC2.Types.LaunchPermissionModifications
import Amazonka.EC2.Types.LaunchSpecification
import Amazonka.EC2.Types.LaunchTemplate
import Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
import Amazonka.EC2.Types.LaunchTemplateAutoRecoveryState
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMapping
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Amazonka.EC2.Types.LaunchTemplateConfig
import Amazonka.EC2.Types.LaunchTemplateCpuOptions
import Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptions
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateErrorCode
import Amazonka.EC2.Types.LaunchTemplateHibernationOptions
import Amazonka.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateHttpTokensState
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataProtocolIpv6
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataTagsState
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateLicenseConfiguration
import Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Amazonka.EC2.Types.LaunchTemplateOverrides
import Amazonka.EC2.Types.LaunchTemplatePlacement
import Amazonka.EC2.Types.LaunchTemplatePlacementRequest
import Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptions
import Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateSpecification
import Amazonka.EC2.Types.LaunchTemplateSpotMarketOptions
import Amazonka.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateTagSpecification
import Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateVersion
import Amazonka.EC2.Types.LaunchTemplatesMonitoring
import Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest
import Amazonka.EC2.Types.LicenseConfiguration
import Amazonka.EC2.Types.LicenseConfigurationRequest
import Amazonka.EC2.Types.ListingState
import Amazonka.EC2.Types.ListingStatus
import Amazonka.EC2.Types.LoadBalancersConfig
import Amazonka.EC2.Types.LoadPermission
import Amazonka.EC2.Types.LoadPermissionModifications
import Amazonka.EC2.Types.LoadPermissionRequest
import Amazonka.EC2.Types.LocalGateway
import Amazonka.EC2.Types.LocalGatewayRoute
import Amazonka.EC2.Types.LocalGatewayRouteState
import Amazonka.EC2.Types.LocalGatewayRouteTable
import Amazonka.EC2.Types.LocalGatewayRouteTableMode
import Amazonka.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Amazonka.EC2.Types.LocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.Types.LocalGatewayRouteType
import Amazonka.EC2.Types.LocalGatewayVirtualInterface
import Amazonka.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Amazonka.EC2.Types.LocalStorage
import Amazonka.EC2.Types.LocalStorageType
import Amazonka.EC2.Types.LocationType
import Amazonka.EC2.Types.LogDestinationType
import Amazonka.EC2.Types.ManagedPrefixList
import Amazonka.EC2.Types.MarketType
import Amazonka.EC2.Types.MembershipType
import Amazonka.EC2.Types.MemoryGiBPerVCpu
import Amazonka.EC2.Types.MemoryGiBPerVCpuRequest
import Amazonka.EC2.Types.MemoryInfo
import Amazonka.EC2.Types.MemoryMiB
import Amazonka.EC2.Types.MemoryMiBRequest
import Amazonka.EC2.Types.MetricPoint
import Amazonka.EC2.Types.MetricType
import Amazonka.EC2.Types.ModifyAvailabilityZoneOptInStatus
import Amazonka.EC2.Types.ModifyTransitGatewayOptions
import Amazonka.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions
import Amazonka.EC2.Types.ModifyVpnTunnelOptionsSpecification
import Amazonka.EC2.Types.Monitoring
import Amazonka.EC2.Types.MonitoringState
import Amazonka.EC2.Types.MoveStatus
import Amazonka.EC2.Types.MovingAddressStatus
import Amazonka.EC2.Types.MulticastSupportValue
import Amazonka.EC2.Types.NatGateway
import Amazonka.EC2.Types.NatGatewayAddress
import Amazonka.EC2.Types.NatGatewayState
import Amazonka.EC2.Types.NetworkAcl
import Amazonka.EC2.Types.NetworkAclAssociation
import Amazonka.EC2.Types.NetworkAclEntry
import Amazonka.EC2.Types.NetworkBandwidthGbps
import Amazonka.EC2.Types.NetworkBandwidthGbpsRequest
import Amazonka.EC2.Types.NetworkCardInfo
import Amazonka.EC2.Types.NetworkInfo
import Amazonka.EC2.Types.NetworkInsightsAccessScope
import Amazonka.EC2.Types.NetworkInsightsAccessScopeAnalysis
import Amazonka.EC2.Types.NetworkInsightsAccessScopeContent
import Amazonka.EC2.Types.NetworkInsightsAnalysis
import Amazonka.EC2.Types.NetworkInsightsPath
import Amazonka.EC2.Types.NetworkInterface
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import Amazonka.EC2.Types.NetworkInterfaceAttachment
import Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges
import Amazonka.EC2.Types.NetworkInterfaceAttribute
import Amazonka.EC2.Types.NetworkInterfaceCount
import Amazonka.EC2.Types.NetworkInterfaceCountRequest
import Amazonka.EC2.Types.NetworkInterfaceCreationType
import Amazonka.EC2.Types.NetworkInterfaceIpv6Address
import Amazonka.EC2.Types.NetworkInterfacePermission
import Amazonka.EC2.Types.NetworkInterfacePermissionState
import Amazonka.EC2.Types.NetworkInterfacePermissionStateCode
import Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress
import Amazonka.EC2.Types.NetworkInterfaceStatus
import Amazonka.EC2.Types.NetworkInterfaceType
import Amazonka.EC2.Types.NewDhcpConfiguration
import Amazonka.EC2.Types.OfferingClassType
import Amazonka.EC2.Types.OfferingTypeValues
import Amazonka.EC2.Types.OidcOptions
import Amazonka.EC2.Types.OnDemandAllocationStrategy
import Amazonka.EC2.Types.OnDemandOptions
import Amazonka.EC2.Types.OnDemandOptionsRequest
import Amazonka.EC2.Types.OperationType
import Amazonka.EC2.Types.PacketHeaderStatement
import Amazonka.EC2.Types.PacketHeaderStatementRequest
import Amazonka.EC2.Types.PartitionLoadFrequency
import Amazonka.EC2.Types.PathComponent
import Amazonka.EC2.Types.PathStatement
import Amazonka.EC2.Types.PathStatementRequest
import Amazonka.EC2.Types.PayerResponsibility
import Amazonka.EC2.Types.PaymentOption
import Amazonka.EC2.Types.PciId
import Amazonka.EC2.Types.PeeringAttachmentStatus
import Amazonka.EC2.Types.PeeringConnectionOptions
import Amazonka.EC2.Types.PeeringConnectionOptionsRequest
import Amazonka.EC2.Types.PeeringTgwInfo
import Amazonka.EC2.Types.PeriodType
import Amazonka.EC2.Types.PermissionGroup
import Amazonka.EC2.Types.Phase1DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.Placement
import Amazonka.EC2.Types.PlacementGroup
import Amazonka.EC2.Types.PlacementGroupInfo
import Amazonka.EC2.Types.PlacementGroupState
import Amazonka.EC2.Types.PlacementGroupStrategy
import Amazonka.EC2.Types.PlacementResponse
import Amazonka.EC2.Types.PlacementStrategy
import Amazonka.EC2.Types.PlatformValues
import Amazonka.EC2.Types.PoolCidrBlock
import Amazonka.EC2.Types.PortRange
import Amazonka.EC2.Types.PrefixList
import Amazonka.EC2.Types.PrefixListAssociation
import Amazonka.EC2.Types.PrefixListEntry
import Amazonka.EC2.Types.PrefixListId
import Amazonka.EC2.Types.PrefixListState
import Amazonka.EC2.Types.PriceSchedule
import Amazonka.EC2.Types.PriceScheduleSpecification
import Amazonka.EC2.Types.PricingDetail
import Amazonka.EC2.Types.PrincipalIdFormat
import Amazonka.EC2.Types.PrincipalType
import Amazonka.EC2.Types.PrivateDnsDetails
import Amazonka.EC2.Types.PrivateDnsNameConfiguration
import Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch
import Amazonka.EC2.Types.PrivateDnsNameOptionsRequest
import Amazonka.EC2.Types.PrivateDnsNameOptionsResponse
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import Amazonka.EC2.Types.ProcessorInfo
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.ProductCodeValues
import Amazonka.EC2.Types.PropagatingVgw
import Amazonka.EC2.Types.Protocol
import Amazonka.EC2.Types.ProtocolValue
import Amazonka.EC2.Types.ProvisionedBandwidth
import Amazonka.EC2.Types.PtrUpdateStatus
import Amazonka.EC2.Types.PublicIpv4Pool
import Amazonka.EC2.Types.PublicIpv4PoolRange
import Amazonka.EC2.Types.Purchase
import Amazonka.EC2.Types.PurchaseRequest
import Amazonka.EC2.Types.RIProductDescription
import Amazonka.EC2.Types.RecurringCharge
import Amazonka.EC2.Types.RecurringChargeFrequency
import Amazonka.EC2.Types.ReferencedSecurityGroup
import Amazonka.EC2.Types.RegionInfo
import Amazonka.EC2.Types.RegisterInstanceTagAttributeRequest
import Amazonka.EC2.Types.RemoveIpamOperatingRegion
import Amazonka.EC2.Types.RemovePrefixListEntry
import Amazonka.EC2.Types.ReplaceRootVolumeTask
import Amazonka.EC2.Types.ReplaceRootVolumeTaskState
import Amazonka.EC2.Types.ReplacementStrategy
import Amazonka.EC2.Types.ReportInstanceReasonCodes
import Amazonka.EC2.Types.ReportStatusType
import Amazonka.EC2.Types.RequestIpamResourceTag
import Amazonka.EC2.Types.RequestLaunchTemplateData
import Amazonka.EC2.Types.RequestSpotLaunchSpecification
import Amazonka.EC2.Types.Reservation
import Amazonka.EC2.Types.ReservationFleetInstanceSpecification
import Amazonka.EC2.Types.ReservationState
import Amazonka.EC2.Types.ReservationValue
import Amazonka.EC2.Types.ReservedInstanceLimitPrice
import Amazonka.EC2.Types.ReservedInstanceReservationValue
import Amazonka.EC2.Types.ReservedInstanceState
import Amazonka.EC2.Types.ReservedInstances
import Amazonka.EC2.Types.ReservedInstancesConfiguration
import Amazonka.EC2.Types.ReservedInstancesId
import Amazonka.EC2.Types.ReservedInstancesListing
import Amazonka.EC2.Types.ReservedInstancesModification
import Amazonka.EC2.Types.ReservedInstancesModificationResult
import Amazonka.EC2.Types.ReservedInstancesOffering
import Amazonka.EC2.Types.ResetFpgaImageAttributeName
import Amazonka.EC2.Types.ResetImageAttributeName
import Amazonka.EC2.Types.ResourceStatement
import Amazonka.EC2.Types.ResourceStatementRequest
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.ResponseError
import Amazonka.EC2.Types.ResponseLaunchTemplateData
import Amazonka.EC2.Types.RootDeviceType
import Amazonka.EC2.Types.Route
import Amazonka.EC2.Types.RouteOrigin
import Amazonka.EC2.Types.RouteState
import Amazonka.EC2.Types.RouteTable
import Amazonka.EC2.Types.RouteTableAssociation
import Amazonka.EC2.Types.RouteTableAssociationState
import Amazonka.EC2.Types.RouteTableAssociationStateCode
import Amazonka.EC2.Types.RuleAction
import Amazonka.EC2.Types.RunInstancesMonitoringEnabled
import Amazonka.EC2.Types.S3ObjectTag
import Amazonka.EC2.Types.S3Storage
import Amazonka.EC2.Types.ScheduledInstance
import Amazonka.EC2.Types.ScheduledInstanceAvailability
import Amazonka.EC2.Types.ScheduledInstanceRecurrence
import Amazonka.EC2.Types.ScheduledInstanceRecurrenceRequest
import Amazonka.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Amazonka.EC2.Types.ScheduledInstancesEbs
import Amazonka.EC2.Types.ScheduledInstancesIamInstanceProfile
import Amazonka.EC2.Types.ScheduledInstancesIpv6Address
import Amazonka.EC2.Types.ScheduledInstancesLaunchSpecification
import Amazonka.EC2.Types.ScheduledInstancesMonitoring
import Amazonka.EC2.Types.ScheduledInstancesNetworkInterface
import Amazonka.EC2.Types.ScheduledInstancesPlacement
import Amazonka.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import Amazonka.EC2.Types.Scope
import Amazonka.EC2.Types.SecurityGroup
import Amazonka.EC2.Types.SecurityGroupIdentifier
import Amazonka.EC2.Types.SecurityGroupReference
import Amazonka.EC2.Types.SecurityGroupRule
import Amazonka.EC2.Types.SecurityGroupRuleDescription
import Amazonka.EC2.Types.SecurityGroupRuleRequest
import Amazonka.EC2.Types.SecurityGroupRuleUpdate
import Amazonka.EC2.Types.SelfServicePortal
import Amazonka.EC2.Types.ServiceConfiguration
import Amazonka.EC2.Types.ServiceConnectivityType
import Amazonka.EC2.Types.ServiceDetail
import Amazonka.EC2.Types.ServiceState
import Amazonka.EC2.Types.ServiceType
import Amazonka.EC2.Types.ServiceTypeDetail
import Amazonka.EC2.Types.ShutdownBehavior
import Amazonka.EC2.Types.SlotDateTimeRangeRequest
import Amazonka.EC2.Types.SlotStartTimeRangeRequest
import Amazonka.EC2.Types.Snapshot
import Amazonka.EC2.Types.SnapshotAttributeName
import Amazonka.EC2.Types.SnapshotDetail
import Amazonka.EC2.Types.SnapshotDiskContainer
import Amazonka.EC2.Types.SnapshotInfo
import Amazonka.EC2.Types.SnapshotRecycleBinInfo
import Amazonka.EC2.Types.SnapshotState
import Amazonka.EC2.Types.SnapshotTaskDetail
import Amazonka.EC2.Types.SnapshotTierStatus
import Amazonka.EC2.Types.SpotAllocationStrategy
import Amazonka.EC2.Types.SpotCapacityRebalance
import Amazonka.EC2.Types.SpotDatafeedSubscription
import Amazonka.EC2.Types.SpotFleetLaunchSpecification
import Amazonka.EC2.Types.SpotFleetMonitoring
import Amazonka.EC2.Types.SpotFleetRequestConfig
import Amazonka.EC2.Types.SpotFleetRequestConfigData
import Amazonka.EC2.Types.SpotFleetTagSpecification
import Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
import Amazonka.EC2.Types.SpotInstanceRequest
import Amazonka.EC2.Types.SpotInstanceState
import Amazonka.EC2.Types.SpotInstanceStateFault
import Amazonka.EC2.Types.SpotInstanceStatus
import Amazonka.EC2.Types.SpotInstanceType
import Amazonka.EC2.Types.SpotMaintenanceStrategies
import Amazonka.EC2.Types.SpotMarketOptions
import Amazonka.EC2.Types.SpotOptions
import Amazonka.EC2.Types.SpotOptionsRequest
import Amazonka.EC2.Types.SpotPlacement
import Amazonka.EC2.Types.SpotPlacementScore
import Amazonka.EC2.Types.SpotPrice
import Amazonka.EC2.Types.SpreadLevel
import Amazonka.EC2.Types.StaleIpPermission
import Amazonka.EC2.Types.StaleSecurityGroup
import Amazonka.EC2.Types.State
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.StaticSourcesSupportValue
import Amazonka.EC2.Types.StatisticType
import Amazonka.EC2.Types.StatusName
import Amazonka.EC2.Types.StatusType
import Amazonka.EC2.Types.Storage
import Amazonka.EC2.Types.StorageLocation
import Amazonka.EC2.Types.StorageTier
import Amazonka.EC2.Types.StoreImageTaskResult
import Amazonka.EC2.Types.Subnet
import Amazonka.EC2.Types.SubnetAssociation
import Amazonka.EC2.Types.SubnetCidrBlockState
import Amazonka.EC2.Types.SubnetCidrBlockStateCode
import Amazonka.EC2.Types.SubnetCidrReservation
import Amazonka.EC2.Types.SubnetCidrReservationType
import Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation
import Amazonka.EC2.Types.SubnetState
import Amazonka.EC2.Types.Subscription
import Amazonka.EC2.Types.SuccessfulInstanceCreditSpecificationItem
import Amazonka.EC2.Types.SuccessfulQueuedPurchaseDeletion
import Amazonka.EC2.Types.SummaryStatus
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TagDescription
import Amazonka.EC2.Types.TagSpecification
import Amazonka.EC2.Types.TargetCapacitySpecification
import Amazonka.EC2.Types.TargetCapacitySpecificationRequest
import Amazonka.EC2.Types.TargetCapacityUnitType
import Amazonka.EC2.Types.TargetConfiguration
import Amazonka.EC2.Types.TargetConfigurationRequest
import Amazonka.EC2.Types.TargetGroup
import Amazonka.EC2.Types.TargetGroupsConfig
import Amazonka.EC2.Types.TargetNetwork
import Amazonka.EC2.Types.TargetReservationValue
import Amazonka.EC2.Types.TargetStorageTier
import Amazonka.EC2.Types.TelemetryStatus
import Amazonka.EC2.Types.Tenancy
import Amazonka.EC2.Types.TerminateConnectionStatus
import Amazonka.EC2.Types.ThroughResourcesStatement
import Amazonka.EC2.Types.ThroughResourcesStatementRequest
import Amazonka.EC2.Types.TieringOperationStatus
import Amazonka.EC2.Types.TotalLocalStorageGB
import Amazonka.EC2.Types.TotalLocalStorageGBRequest
import Amazonka.EC2.Types.TpmSupportValues
import Amazonka.EC2.Types.TrafficDirection
import Amazonka.EC2.Types.TrafficMirrorFilter
import Amazonka.EC2.Types.TrafficMirrorFilterRule
import Amazonka.EC2.Types.TrafficMirrorFilterRuleField
import Amazonka.EC2.Types.TrafficMirrorNetworkService
import Amazonka.EC2.Types.TrafficMirrorPortRange
import Amazonka.EC2.Types.TrafficMirrorPortRangeRequest
import Amazonka.EC2.Types.TrafficMirrorRuleAction
import Amazonka.EC2.Types.TrafficMirrorSession
import Amazonka.EC2.Types.TrafficMirrorSessionField
import Amazonka.EC2.Types.TrafficMirrorTarget
import Amazonka.EC2.Types.TrafficMirrorTargetType
import Amazonka.EC2.Types.TrafficType
import Amazonka.EC2.Types.TransitGateway
import Amazonka.EC2.Types.TransitGatewayAssociation
import Amazonka.EC2.Types.TransitGatewayAssociationState
import Amazonka.EC2.Types.TransitGatewayAttachment
import Amazonka.EC2.Types.TransitGatewayAttachmentAssociation
import Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import Amazonka.EC2.Types.TransitGatewayAttachmentPropagation
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import Amazonka.EC2.Types.TransitGatewayAttachmentState
import Amazonka.EC2.Types.TransitGatewayConnect
import Amazonka.EC2.Types.TransitGatewayConnectOptions
import Amazonka.EC2.Types.TransitGatewayConnectPeer
import Amazonka.EC2.Types.TransitGatewayConnectPeerConfiguration
import Amazonka.EC2.Types.TransitGatewayConnectPeerState
import Amazonka.EC2.Types.TransitGatewayConnectRequestBgpOptions
import Amazonka.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Amazonka.EC2.Types.TransitGatewayMulticastDomain
import Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociation
import Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociations
import Amazonka.EC2.Types.TransitGatewayMulticastDomainOptions
import Amazonka.EC2.Types.TransitGatewayMulticastDomainState
import Amazonka.EC2.Types.TransitGatewayMulticastGroup
import Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
import Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
import Amazonka.EC2.Types.TransitGatewayOptions
import Amazonka.EC2.Types.TransitGatewayPeeringAttachment
import Amazonka.EC2.Types.TransitGatewayPeeringAttachmentOptions
import Amazonka.EC2.Types.TransitGatewayPolicyRule
import Amazonka.EC2.Types.TransitGatewayPolicyRuleMetaData
import Amazonka.EC2.Types.TransitGatewayPolicyTable
import Amazonka.EC2.Types.TransitGatewayPolicyTableAssociation
import Amazonka.EC2.Types.TransitGatewayPolicyTableEntry
import Amazonka.EC2.Types.TransitGatewayPolicyTableState
import Amazonka.EC2.Types.TransitGatewayPrefixListAttachment
import Amazonka.EC2.Types.TransitGatewayPrefixListReference
import Amazonka.EC2.Types.TransitGatewayPrefixListReferenceState
import Amazonka.EC2.Types.TransitGatewayPropagation
import Amazonka.EC2.Types.TransitGatewayPropagationState
import Amazonka.EC2.Types.TransitGatewayRequestOptions
import Amazonka.EC2.Types.TransitGatewayRoute
import Amazonka.EC2.Types.TransitGatewayRouteAttachment
import Amazonka.EC2.Types.TransitGatewayRouteState
import Amazonka.EC2.Types.TransitGatewayRouteTable
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncement
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementDirection
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementState
import Amazonka.EC2.Types.TransitGatewayRouteTableAssociation
import Amazonka.EC2.Types.TransitGatewayRouteTablePropagation
import Amazonka.EC2.Types.TransitGatewayRouteTableRoute
import Amazonka.EC2.Types.TransitGatewayRouteTableState
import Amazonka.EC2.Types.TransitGatewayRouteType
import Amazonka.EC2.Types.TransitGatewayState
import Amazonka.EC2.Types.TransitGatewayVpcAttachment
import Amazonka.EC2.Types.TransitGatewayVpcAttachmentOptions
import Amazonka.EC2.Types.TransportProtocol
import Amazonka.EC2.Types.TrunkInterfaceAssociation
import Amazonka.EC2.Types.TrustProviderType
import Amazonka.EC2.Types.TunnelInsideIpVersion
import Amazonka.EC2.Types.TunnelOption
import Amazonka.EC2.Types.UnlimitedSupportedInstanceFamily
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Amazonka.EC2.Types.UnsuccessfulItem
import Amazonka.EC2.Types.UnsuccessfulItemError
import Amazonka.EC2.Types.UsageClassType
import Amazonka.EC2.Types.UserBucket
import Amazonka.EC2.Types.UserBucketDetails
import Amazonka.EC2.Types.UserData
import Amazonka.EC2.Types.UserIdGroupPair
import Amazonka.EC2.Types.UserTrustProviderType
import Amazonka.EC2.Types.VCpuCountRange
import Amazonka.EC2.Types.VCpuCountRangeRequest
import Amazonka.EC2.Types.VCpuInfo
import Amazonka.EC2.Types.ValidationError
import Amazonka.EC2.Types.ValidationWarning
import Amazonka.EC2.Types.VerifiedAccessEndpoint
import Amazonka.EC2.Types.VerifiedAccessEndpointAttachmentType
import Amazonka.EC2.Types.VerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import Amazonka.EC2.Types.VerifiedAccessEndpointStatus
import Amazonka.EC2.Types.VerifiedAccessEndpointStatusCode
import Amazonka.EC2.Types.VerifiedAccessEndpointType
import Amazonka.EC2.Types.VerifiedAccessGroup
import Amazonka.EC2.Types.VerifiedAccessInstance
import Amazonka.EC2.Types.VerifiedAccessInstanceLoggingConfiguration
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestination
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatusCode
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestination
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogOptions
import Amazonka.EC2.Types.VerifiedAccessLogS3Destination
import Amazonka.EC2.Types.VerifiedAccessLogS3DestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogs
import Amazonka.EC2.Types.VerifiedAccessTrustProvider
import Amazonka.EC2.Types.VerifiedAccessTrustProviderCondensed
import Amazonka.EC2.Types.VgwTelemetry
import Amazonka.EC2.Types.VirtualizationType
import Amazonka.EC2.Types.Volume
import Amazonka.EC2.Types.VolumeAttachment
import Amazonka.EC2.Types.VolumeAttachmentState
import Amazonka.EC2.Types.VolumeAttributeName
import Amazonka.EC2.Types.VolumeDetail
import Amazonka.EC2.Types.VolumeModification
import Amazonka.EC2.Types.VolumeModificationState
import Amazonka.EC2.Types.VolumeState
import Amazonka.EC2.Types.VolumeStatusAction
import Amazonka.EC2.Types.VolumeStatusAttachmentStatus
import Amazonka.EC2.Types.VolumeStatusDetails
import Amazonka.EC2.Types.VolumeStatusEvent
import Amazonka.EC2.Types.VolumeStatusInfo
import Amazonka.EC2.Types.VolumeStatusInfoStatus
import Amazonka.EC2.Types.VolumeStatusItem
import Amazonka.EC2.Types.VolumeStatusName
import Amazonka.EC2.Types.VolumeType
import Amazonka.EC2.Types.Vpc
import Amazonka.EC2.Types.VpcAttachment
import Amazonka.EC2.Types.VpcAttributeName
import Amazonka.EC2.Types.VpcCidrBlockAssociation
import Amazonka.EC2.Types.VpcCidrBlockState
import Amazonka.EC2.Types.VpcCidrBlockStateCode
import Amazonka.EC2.Types.VpcClassicLink
import Amazonka.EC2.Types.VpcEndpoint
import Amazonka.EC2.Types.VpcEndpointConnection
import Amazonka.EC2.Types.VpcEndpointType
import Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation
import Amazonka.EC2.Types.VpcPeeringConnection
import Amazonka.EC2.Types.VpcPeeringConnectionOptionsDescription
import Amazonka.EC2.Types.VpcPeeringConnectionStateReason
import Amazonka.EC2.Types.VpcPeeringConnectionStateReasonCode
import Amazonka.EC2.Types.VpcPeeringConnectionVpcInfo
import Amazonka.EC2.Types.VpcState
import Amazonka.EC2.Types.VpcTenancy
import Amazonka.EC2.Types.VpnConnection
import Amazonka.EC2.Types.VpnConnectionDeviceType
import Amazonka.EC2.Types.VpnConnectionOptions
import Amazonka.EC2.Types.VpnConnectionOptionsSpecification
import Amazonka.EC2.Types.VpnEcmpSupportValue
import Amazonka.EC2.Types.VpnGateway
import Amazonka.EC2.Types.VpnProtocol
import Amazonka.EC2.Types.VpnState
import Amazonka.EC2.Types.VpnStaticRoute
import Amazonka.EC2.Types.VpnStaticRouteSource
import Amazonka.EC2.Types.VpnTunnelLogOptions
import Amazonka.EC2.Types.VpnTunnelLogOptionsSpecification
import Amazonka.EC2.Types.VpnTunnelOptionsSpecification
import Amazonka.EC2.Types.WeekDay
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "EC2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ec2",
      Core.signingName = "ec2",
      Core.version = "2016-11-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "EC2",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has
          ( Core.hasCode "EC2ThrottledException"
              Prelude.. Core.hasStatus 503
          )
          e =
          Prelude.Just "ec2_throttled_exception"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestLimitExceeded"
              Prelude.. Core.hasStatus 503
          )
          e =
          Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing
