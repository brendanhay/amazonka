{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- * TunnelInsideIpVersion
    TunnelInsideIpVersion (..),

    -- * UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- * UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- * UsageClassType
    UsageClassType (..),

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
    accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeId,
    accessScopeAnalysisFinding_findingId,

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
    accountAttribute_attributeValues,
    accountAttribute_attributeName,

    -- * AccountAttributeValue
    AccountAttributeValue (..),
    newAccountAttributeValue,
    accountAttributeValue_attributeValue,

    -- * ActiveInstance
    ActiveInstance (..),
    newActiveInstance,
    activeInstance_spotInstanceRequestId,
    activeInstance_instanceHealth,
    activeInstance_instanceType,
    activeInstance_instanceId,

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
    addedPrincipal_servicePermissionId,
    addedPrincipal_principalType,
    addedPrincipal_serviceId,

    -- * AdditionalDetail
    AdditionalDetail (..),
    newAdditionalDetail,
    additionalDetail_additionalDetailType,
    additionalDetail_component,

    -- * Address
    Address (..),
    newAddress,
    address_tags,
    address_allocationId,
    address_networkBorderGroup,
    address_domain,
    address_networkInterfaceOwnerId,
    address_carrierIp,
    address_customerOwnedIpv4Pool,
    address_publicIp,
    address_instanceId,
    address_networkInterfaceId,
    address_privateIpAddress,
    address_customerOwnedIp,
    address_associationId,
    address_publicIpv4Pool,

    -- * AddressAttribute
    AddressAttribute (..),
    newAddressAttribute,
    addressAttribute_ptrRecord,
    addressAttribute_allocationId,
    addressAttribute_publicIp,
    addressAttribute_ptrRecordUpdate,

    -- * AddressTransfer
    AddressTransfer (..),
    newAddressTransfer,
    addressTransfer_allocationId,
    addressTransfer_transferOfferExpirationTimestamp,
    addressTransfer_transferOfferAcceptedTimestamp,
    addressTransfer_transferAccountId,
    addressTransfer_publicIp,
    addressTransfer_addressTransferStatus,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    newAllowedPrincipal,
    allowedPrincipal_tags,
    allowedPrincipal_principal,
    allowedPrincipal_servicePermissionId,
    allowedPrincipal_principalType,
    allowedPrincipal_serviceId,

    -- * AlternatePathHint
    AlternatePathHint (..),
    newAlternatePathHint,
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- * AnalysisAclRule
    AnalysisAclRule (..),
    newAnalysisAclRule,
    analysisAclRule_egress,
    analysisAclRule_portRange,
    analysisAclRule_cidr,
    analysisAclRule_ruleNumber,
    analysisAclRule_ruleAction,
    analysisAclRule_protocol,

    -- * AnalysisComponent
    AnalysisComponent (..),
    newAnalysisComponent,
    analysisComponent_name,
    analysisComponent_arn,
    analysisComponent_id,

    -- * AnalysisLoadBalancerListener
    AnalysisLoadBalancerListener (..),
    newAnalysisLoadBalancerListener,
    analysisLoadBalancerListener_instancePort,
    analysisLoadBalancerListener_loadBalancerPort,

    -- * AnalysisLoadBalancerTarget
    AnalysisLoadBalancerTarget (..),
    newAnalysisLoadBalancerTarget,
    analysisLoadBalancerTarget_port,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_instance,

    -- * AnalysisPacketHeader
    AnalysisPacketHeader (..),
    newAnalysisPacketHeader,
    analysisPacketHeader_sourcePortRanges,
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_protocol,
    analysisPacketHeader_destinationPortRanges,

    -- * AnalysisRouteTableRoute
    AnalysisRouteTableRoute (..),
    newAnalysisRouteTableRoute,
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_state,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_origin,

    -- * AnalysisSecurityGroupRule
    AnalysisSecurityGroupRule (..),
    newAnalysisSecurityGroupRule,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_direction,

    -- * AssignedPrivateIpAddress
    AssignedPrivateIpAddress (..),
    newAssignedPrivateIpAddress,
    assignedPrivateIpAddress_privateIpAddress,

    -- * AssociatedRole
    AssociatedRole (..),
    newAssociatedRole,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_associatedRoleArn,
    associatedRole_certificateS3BucketName,

    -- * AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    newAssociatedTargetNetwork,
    associatedTargetNetwork_networkId,
    associatedTargetNetwork_networkType,

    -- * AssociationStatus
    AssociationStatus (..),
    newAssociationStatus,
    associationStatus_message,
    associationStatus_code,

    -- * AthenaIntegration
    AthenaIntegration (..),
    newAthenaIntegration,
    athenaIntegration_partitionEndDate,
    athenaIntegration_partitionStartDate,
    athenaIntegration_integrationResultS3DestinationArn,
    athenaIntegration_partitionLoadFrequency,

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
    authorizationRule_destinationCidr,
    authorizationRule_accessAll,
    authorizationRule_status,
    authorizationRule_description,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_groupId,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_zoneType,
    availabilityZone_parentZoneId,
    availabilityZone_parentZoneName,
    availabilityZone_zoneName,
    availabilityZone_networkBorderGroup,
    availabilityZone_state,
    availabilityZone_optInStatus,
    availabilityZone_groupName,
    availabilityZone_messages,
    availabilityZone_regionName,
    availabilityZone_zoneId,

    -- * AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    newAvailabilityZoneMessage,
    availabilityZoneMessage_message,

    -- * AvailableCapacity
    AvailableCapacity (..),
    newAvailableCapacity,
    availableCapacity_availableVCpus,
    availableCapacity_availableInstanceCapacity,

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
    bundleTaskError_message,
    bundleTaskError_code,

    -- * ByoipCidr
    ByoipCidr (..),
    newByoipCidr,
    byoipCidr_cidr,
    byoipCidr_state,
    byoipCidr_description,
    byoipCidr_statusMessage,

    -- * CancelCapacityReservationFleetError
    CancelCapacityReservationFleetError (..),
    newCancelCapacityReservationFleetError,
    cancelCapacityReservationFleetError_message,
    cancelCapacityReservationFleetError_code,

    -- * CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    newCancelSpotFleetRequestsError,
    cancelSpotFleetRequestsError_message,
    cancelSpotFleetRequestsError_code,

    -- * CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (..),
    newCancelSpotFleetRequestsErrorItem,
    cancelSpotFleetRequestsErrorItem_spotFleetRequestId,
    cancelSpotFleetRequestsErrorItem_error,

    -- * CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (..),
    newCancelSpotFleetRequestsSuccessItem,
    cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,

    -- * CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    newCancelledSpotInstanceRequest,
    cancelledSpotInstanceRequest_spotInstanceRequestId,
    cancelledSpotInstanceRequest_state,

    -- * CapacityAllocation
    CapacityAllocation (..),
    newCapacityAllocation,
    capacityAllocation_count,
    capacityAllocation_allocationType,

    -- * CapacityReservation
    CapacityReservation (..),
    newCapacityReservation,
    capacityReservation_ebsOptimized,
    capacityReservation_tags,
    capacityReservation_capacityReservationFleetId,
    capacityReservation_capacityAllocations,
    capacityReservation_ephemeralStorage,
    capacityReservation_placementGroupArn,
    capacityReservation_outpostArn,
    capacityReservation_ownerId,
    capacityReservation_totalInstanceCount,
    capacityReservation_endDate,
    capacityReservation_state,
    capacityReservation_capacityReservationId,
    capacityReservation_availabilityZone,
    capacityReservation_capacityReservationArn,
    capacityReservation_instanceType,
    capacityReservation_instancePlatform,
    capacityReservation_startDate,
    capacityReservation_createDate,
    capacityReservation_availableInstanceCount,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_endDateType,
    capacityReservation_tenancy,
    capacityReservation_availabilityZoneId,

    -- * CapacityReservationFleet
    CapacityReservationFleet (..),
    newCapacityReservationFleet,
    capacityReservationFleet_tags,
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_endDate,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_state,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_totalTargetCapacity,
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_createTime,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_tenancy,

    -- * CapacityReservationFleetCancellationState
    CapacityReservationFleetCancellationState (..),
    newCapacityReservationFleetCancellationState,
    capacityReservationFleetCancellationState_capacityReservationFleetId,
    capacityReservationFleetCancellationState_currentFleetState,
    capacityReservationFleetCancellationState_previousFleetState,

    -- * CapacityReservationGroup
    CapacityReservationGroup (..),
    newCapacityReservationGroup,
    capacityReservationGroup_ownerId,
    capacityReservationGroup_groupArn,

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
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,
    carrierGateway_ownerId,
    carrierGateway_state,
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
    classicLinkInstance_tags,
    classicLinkInstance_instanceId,
    classicLinkInstance_vpcId,
    classicLinkInstance_groups,

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
    clientCertificateRevocationListStatus_message,
    clientCertificateRevocationListStatus_code,

    -- * ClientConnectOptions
    ClientConnectOptions (..),
    newClientConnectOptions,
    clientConnectOptions_lambdaFunctionArn,
    clientConnectOptions_enabled,

    -- * ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    newClientConnectResponseOptions,
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_lambdaFunctionArn,
    clientConnectResponseOptions_enabled,

    -- * ClientData
    ClientData (..),
    newClientData,
    clientData_uploadEnd,
    clientData_uploadStart,
    clientData_uploadSize,
    clientData_comment,

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
    clientVpnAuthentication_type,
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_activeDirectory,

    -- * ClientVpnAuthenticationRequest
    ClientVpnAuthenticationRequest (..),
    newClientVpnAuthenticationRequest,
    clientVpnAuthenticationRequest_type,
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_activeDirectory,

    -- * ClientVpnAuthorizationRuleStatus
    ClientVpnAuthorizationRuleStatus (..),
    newClientVpnAuthorizationRuleStatus,
    clientVpnAuthorizationRuleStatus_message,
    clientVpnAuthorizationRuleStatus_code,

    -- * ClientVpnConnection
    ClientVpnConnection (..),
    newClientVpnConnection,
    clientVpnConnection_egressBytes,
    clientVpnConnection_username,
    clientVpnConnection_timestamp,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_status,
    clientVpnConnection_connectionId,
    clientVpnConnection_egressPackets,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_clientIp,

    -- * ClientVpnConnectionStatus
    ClientVpnConnectionStatus (..),
    newClientVpnConnectionStatus,
    clientVpnConnectionStatus_message,
    clientVpnConnectionStatus_code,

    -- * ClientVpnEndpoint
    ClientVpnEndpoint (..),
    newClientVpnEndpoint,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_sessionTimeoutHours,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_status,
    clientVpnEndpoint_description,
    clientVpnEndpoint_clientLoginBannerOptions,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_vpnProtocol,

    -- * ClientVpnEndpointAttributeStatus
    ClientVpnEndpointAttributeStatus (..),
    newClientVpnEndpointAttributeStatus,
    clientVpnEndpointAttributeStatus_message,
    clientVpnEndpointAttributeStatus_code,

    -- * ClientVpnEndpointStatus
    ClientVpnEndpointStatus (..),
    newClientVpnEndpointStatus,
    clientVpnEndpointStatus_message,
    clientVpnEndpointStatus_code,

    -- * ClientVpnRoute
    ClientVpnRoute (..),
    newClientVpnRoute,
    clientVpnRoute_type,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_status,
    clientVpnRoute_description,
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_origin,

    -- * ClientVpnRouteStatus
    ClientVpnRouteStatus (..),
    newClientVpnRouteStatus,
    clientVpnRouteStatus_message,
    clientVpnRouteStatus_code,

    -- * CloudWatchLogOptions
    CloudWatchLogOptions (..),
    newCloudWatchLogOptions,
    cloudWatchLogOptions_logGroupArn,
    cloudWatchLogOptions_logEnabled,
    cloudWatchLogOptions_logOutputFormat,

    -- * CloudWatchLogOptionsSpecification
    CloudWatchLogOptionsSpecification (..),
    newCloudWatchLogOptionsSpecification,
    cloudWatchLogOptionsSpecification_logGroupArn,
    cloudWatchLogOptionsSpecification_logEnabled,
    cloudWatchLogOptionsSpecification_logOutputFormat,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    newCoipAddressUsage,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_allocationId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,

    -- * CoipCidr
    CoipCidr (..),
    newCoipCidr,
    coipCidr_localGatewayRouteTableId,
    coipCidr_cidr,
    coipCidr_coipPoolId,

    -- * CoipPool
    CoipPool (..),
    newCoipPool,
    coipPool_tags,
    coipPool_poolArn,
    coipPool_localGatewayRouteTableId,
    coipPool_poolCidrs,
    coipPool_poolId,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    newConnectionLogOptions,
    connectionLogOptions_cloudwatchLogGroup,
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogStream,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    newConnectionLogResponseOptions,
    connectionLogResponseOptions_cloudwatchLogGroup,
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogStream,

    -- * ConnectionNotification
    ConnectionNotification (..),
    newConnectionNotification,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionEvents,
    connectionNotification_vpcEndpointId,
    connectionNotification_connectionNotificationType,
    connectionNotification_connectionNotificationArn,
    connectionNotification_serviceId,

    -- * ConversionTask
    ConversionTask (..),
    newConversionTask,
    conversionTask_tags,
    conversionTask_importInstance,
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_importVolume,

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
    createFleetError_launchTemplateAndOverrides,
    createFleetError_lifecycle,
    createFleetError_errorMessage,
    createFleetError_errorCode,

    -- * CreateFleetInstance
    CreateFleetInstance (..),
    newCreateFleetInstance,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,
    createFleetInstance_platform,
    createFleetInstance_instanceType,
    createFleetInstance_instanceIds,

    -- * CreateTransitGatewayConnectRequestOptions
    CreateTransitGatewayConnectRequestOptions (..),
    newCreateTransitGatewayConnectRequestOptions,
    createTransitGatewayConnectRequestOptions_protocol,

    -- * CreateTransitGatewayMulticastDomainRequestOptions
    CreateTransitGatewayMulticastDomainRequestOptions (..),
    newCreateTransitGatewayMulticastDomainRequestOptions,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,

    -- * CreateTransitGatewayPeeringAttachmentRequestOptions
    CreateTransitGatewayPeeringAttachmentRequestOptions (..),
    newCreateTransitGatewayPeeringAttachmentRequestOptions,
    createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting,

    -- * CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (..),
    newCreateTransitGatewayVpcAttachmentRequestOptions,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,

    -- * CreateVolumePermission
    CreateVolumePermission (..),
    newCreateVolumePermission,
    createVolumePermission_userId,
    createVolumePermission_group,

    -- * CreateVolumePermissionModifications
    CreateVolumePermissionModifications (..),
    newCreateVolumePermissionModifications,
    createVolumePermissionModifications_remove,
    createVolumePermissionModifications_add,

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
    customerGateway_tags,
    customerGateway_deviceName,
    customerGateway_certificateArn,
    customerGateway_bgpAsn,
    customerGateway_customerGatewayId,
    customerGateway_ipAddress,
    customerGateway_state,
    customerGateway_type,

    -- * DeleteFleetError
    DeleteFleetError (..),
    newDeleteFleetError,
    deleteFleetError_message,
    deleteFleetError_code,

    -- * DeleteFleetErrorItem
    DeleteFleetErrorItem (..),
    newDeleteFleetErrorItem,
    deleteFleetErrorItem_fleetId,
    deleteFleetErrorItem_error,

    -- * DeleteFleetSuccessItem
    DeleteFleetSuccessItem (..),
    newDeleteFleetSuccessItem,
    deleteFleetSuccessItem_fleetId,
    deleteFleetSuccessItem_currentFleetState,
    deleteFleetSuccessItem_previousFleetState,

    -- * DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    newDeleteLaunchTemplateVersionsResponseErrorItem,
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,

    -- * DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    newDeleteLaunchTemplateVersionsResponseSuccessItem,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,

    -- * DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (..),
    newDeleteQueuedReservedInstancesError,
    deleteQueuedReservedInstancesError_message,
    deleteQueuedReservedInstancesError_code,

    -- * DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    newDeregisterInstanceTagAttributeRequest,
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,

    -- * DescribeFastLaunchImagesSuccessItem
    DescribeFastLaunchImagesSuccessItem (..),
    newDescribeFastLaunchImagesSuccessItem,
    describeFastLaunchImagesSuccessItem_resourceType,
    describeFastLaunchImagesSuccessItem_ownerId,
    describeFastLaunchImagesSuccessItem_launchTemplate,
    describeFastLaunchImagesSuccessItem_stateTransitionTime,
    describeFastLaunchImagesSuccessItem_state,
    describeFastLaunchImagesSuccessItem_stateTransitionReason,
    describeFastLaunchImagesSuccessItem_snapshotConfiguration,
    describeFastLaunchImagesSuccessItem_maxParallelLaunches,
    describeFastLaunchImagesSuccessItem_imageId,

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    newDescribeFastSnapshotRestoreSuccessItem,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_disablingTime,

    -- * DescribeFleetError
    DescribeFleetError (..),
    newDescribeFleetError,
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_lifecycle,
    describeFleetError_errorMessage,
    describeFleetError_errorCode,

    -- * DescribeFleetsInstances
    DescribeFleetsInstances (..),
    newDescribeFleetsInstances,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_platform,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_instanceIds,

    -- * DestinationOptionsRequest
    DestinationOptionsRequest (..),
    newDestinationOptionsRequest,
    destinationOptionsRequest_perHourPartition,
    destinationOptionsRequest_hiveCompatiblePartitions,
    destinationOptionsRequest_fileFormat,

    -- * DestinationOptionsResponse
    DestinationOptionsResponse (..),
    newDestinationOptionsResponse,
    destinationOptionsResponse_perHourPartition,
    destinationOptionsResponse_hiveCompatiblePartitions,
    destinationOptionsResponse_fileFormat,

    -- * DhcpConfiguration
    DhcpConfiguration (..),
    newDhcpConfiguration,
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- * DhcpOptions
    DhcpOptions (..),
    newDhcpOptions,
    dhcpOptions_tags,
    dhcpOptions_ownerId,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,

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
    disableFastSnapshotRestoreErrorItem_snapshotId,
    disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,

    -- * DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (..),
    newDisableFastSnapshotRestoreStateError,
    disableFastSnapshotRestoreStateError_message,
    disableFastSnapshotRestoreStateError_code,

    -- * DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    newDisableFastSnapshotRestoreStateErrorItem,
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,
    disableFastSnapshotRestoreStateErrorItem_error,

    -- * DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    newDisableFastSnapshotRestoreSuccessItem,
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_disablingTime,

    -- * DiskImage
    DiskImage (..),
    newDiskImage,
    diskImage_description,
    diskImage_volume,
    diskImage_image,

    -- * DiskImageDescription
    DiskImageDescription (..),
    newDiskImageDescription,
    diskImageDescription_importManifestUrl,
    diskImageDescription_format,
    diskImageDescription_size,
    diskImageDescription_checksum,

    -- * DiskImageDetail
    DiskImageDetail (..),
    newDiskImageDetail,
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- * DiskImageVolumeDescription
    DiskImageVolumeDescription (..),
    newDiskImageVolumeDescription,
    diskImageVolumeDescription_size,
    diskImageVolumeDescription_id,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_type,
    diskInfo_sizeInGB,
    diskInfo_count,

    -- * DnsEntry
    DnsEntry (..),
    newDnsEntry,
    dnsEntry_hostedZoneId,
    dnsEntry_dnsName,

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
    dnsServersOptionsModifyStructure_enabled,
    dnsServersOptionsModifyStructure_customDnsServers,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_throughput,
    ebsBlockDevice_iops,

    -- * EbsInfo
    EbsInfo (..),
    newEbsInfo,
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_nvmeSupport,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    newEbsInstanceBlockDevice,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_volumeId,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- * EbsOptimizedInfo
    EbsOptimizedInfo (..),
    newEbsOptimizedInfo,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- * EfaInfo
    EfaInfo (..),
    newEfaInfo,
    efaInfo_maximumEfaInterfaces,

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    newEgressOnlyInternetGateway,
    egressOnlyInternetGateway_tags,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_attachments,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    newElasticGpuAssociation,
    elasticGpuAssociation_elasticGpuAssociationState,
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuAssociationId,

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
    elasticGpus_tags,
    elasticGpus_elasticGpuHealth,
    elasticGpus_availabilityZone,
    elasticGpus_elasticGpuState,
    elasticGpus_instanceId,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuType,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    newElasticInferenceAccelerator,
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    newElasticInferenceAcceleratorAssociation,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- * EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    newEnableFastSnapshotRestoreErrorItem,
    enableFastSnapshotRestoreErrorItem_snapshotId,
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,

    -- * EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (..),
    newEnableFastSnapshotRestoreStateError,
    enableFastSnapshotRestoreStateError_message,
    enableFastSnapshotRestoreStateError_code,

    -- * EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (..),
    newEnableFastSnapshotRestoreStateErrorItem,
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,
    enableFastSnapshotRestoreStateErrorItem_error,

    -- * EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    newEnableFastSnapshotRestoreSuccessItem,
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_disablingTime,

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
    eventInformation_instanceId,
    eventInformation_eventSubType,
    eventInformation_eventDescription,

    -- * Explanation
    Explanation (..),
    newExplanation,
    explanation_destination,
    explanation_port,
    explanation_vpnConnection,
    explanation_loadBalancerListenerPort,
    explanation_transitGatewayAttachment,
    explanation_componentAccount,
    explanation_missingComponent,
    explanation_routeTableRoute,
    explanation_subnetRouteTable,
    explanation_vpc,
    explanation_natGateway,
    explanation_classicLoadBalancerListener,
    explanation_loadBalancerTargetGroup,
    explanation_securityGroup,
    explanation_acl,
    explanation_routeTable,
    explanation_internetGateway,
    explanation_attachedTo,
    explanation_availabilityZones,
    explanation_cidrs,
    explanation_state,
    explanation_loadBalancerTarget,
    explanation_customerGateway,
    explanation_vpnGateway,
    explanation_protocols,
    explanation_componentRegion,
    explanation_loadBalancerArn,
    explanation_sourceVpc,
    explanation_ingressRouteTable,
    explanation_aclRule,
    explanation_vpcPeeringConnection,
    explanation_address,
    explanation_loadBalancerTargetPort,
    explanation_securityGroups,
    explanation_transitGateway,
    explanation_destinationVpc,
    explanation_addresses,
    explanation_prefixList,
    explanation_elasticLoadBalancerListener,
    explanation_subnet,
    explanation_packetField,
    explanation_explanationCode,
    explanation_networkInterface,
    explanation_direction,
    explanation_transitGatewayRouteTableRoute,
    explanation_loadBalancerTargetGroups,
    explanation_portRanges,
    explanation_vpcEndpoint,
    explanation_transitGatewayRouteTable,
    explanation_component,
    explanation_securityGroupRule,

    -- * ExportImageTask
    ExportImageTask (..),
    newExportImageTask,
    exportImageTask_tags,
    exportImageTask_progress,
    exportImageTask_exportImageTaskId,
    exportImageTask_s3ExportLocation,
    exportImageTask_status,
    exportImageTask_description,
    exportImageTask_statusMessage,
    exportImageTask_imageId,

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
    exportToS3Task_s3Bucket,
    exportToS3Task_s3Key,
    exportToS3Task_diskImageFormat,
    exportToS3Task_containerFormat,

    -- * ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    newExportToS3TaskSpecification,
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_diskImageFormat,
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_s3Prefix,

    -- * FailedCapacityReservationFleetCancellationResult
    FailedCapacityReservationFleetCancellationResult (..),
    newFailedCapacityReservationFleetCancellationResult,
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    newFailedQueuedPurchaseDeletion,
    failedQueuedPurchaseDeletion_reservedInstancesId,
    failedQueuedPurchaseDeletion_error,

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
    fastLaunchLaunchTemplateSpecificationResponse_version,
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName,

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
    federatedAuthentication_selfServiceSamlProviderArn,
    federatedAuthentication_samlProviderArn,

    -- * FederatedAuthenticationRequest
    FederatedAuthenticationRequest (..),
    newFederatedAuthenticationRequest,
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,
    federatedAuthenticationRequest_sAMLProviderArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * FleetCapacityReservation
    FleetCapacityReservation (..),
    newFleetCapacityReservation,
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_totalInstanceCount,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_priority,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_weight,
    fleetCapacityReservation_availabilityZoneId,

    -- * FleetData
    FleetData (..),
    newFleetData,
    fleetData_tags,
    fleetData_instances,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_fleetId,
    fleetData_clientToken,
    fleetData_type,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_targetCapacitySpecification,
    fleetData_activityStatus,
    fleetData_onDemandOptions,
    fleetData_context,
    fleetData_fulfilledCapacity,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_launchTemplateConfigs,
    fleetData_errors,
    fleetData_fleetState,
    fleetData_spotOptions,
    fleetData_terminateInstancesWithExpiration,
    fleetData_validUntil,
    fleetData_createTime,

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
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_instanceRequirements,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_weightedCapacity,
    fleetLaunchTemplateOverrides_imageId,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    newFleetLaunchTemplateOverridesRequest,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_instanceRequirements,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_maxPrice,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,
    fleetLaunchTemplateOverridesRequest_imageId,

    -- * FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    newFleetLaunchTemplateSpecification,
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_version,
    fleetLaunchTemplateSpecification_launchTemplateName,

    -- * FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    newFleetLaunchTemplateSpecificationRequest,
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
    fleetLaunchTemplateSpecificationRequest_version,
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,

    -- * FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (..),
    newFleetSpotCapacityRebalance,
    fleetSpotCapacityRebalance_terminationDelay,
    fleetSpotCapacityRebalance_replacementStrategy,

    -- * FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    newFleetSpotCapacityRebalanceRequest,
    fleetSpotCapacityRebalanceRequest_terminationDelay,
    fleetSpotCapacityRebalanceRequest_replacementStrategy,

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
    flowLog_resourceId,
    flowLog_tags,
    flowLog_destinationOptions,
    flowLog_flowLogId,
    flowLog_trafficType,
    flowLog_flowLogStatus,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsErrorMessage,
    flowLog_logFormat,
    flowLog_logDestination,
    flowLog_deliverCrossAccountRole,
    flowLog_logDestinationType,
    flowLog_creationTime,
    flowLog_maxAggregationInterval,
    flowLog_deliverLogsStatus,
    flowLog_logGroupName,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    newFpgaDeviceInfo,
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_name,
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_manufacturer,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    newFpgaDeviceMemoryInfo,
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- * FpgaImage
    FpgaImage (..),
    newFpgaImage,
    fpgaImage_tags,
    fpgaImage_ownerAlias,
    fpgaImage_name,
    fpgaImage_ownerId,
    fpgaImage_shellVersion,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_state,
    fpgaImage_productCodes,
    fpgaImage_description,
    fpgaImage_fpgaImageId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_updateTime,
    fpgaImage_createTime,
    fpgaImage_pciId,
    fpgaImage_public,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    newFpgaImageAttribute,
    fpgaImageAttribute_name,
    fpgaImageAttribute_loadPermissions,
    fpgaImageAttribute_productCodes,
    fpgaImageAttribute_description,
    fpgaImageAttribute_fpgaImageId,

    -- * FpgaImageState
    FpgaImageState (..),
    newFpgaImageState,
    fpgaImageState_message,
    fpgaImageState_code,

    -- * FpgaInfo
    FpgaInfo (..),
    newFpgaInfo,
    fpgaInfo_totalFpgaMemoryInMiB,
    fpgaInfo_fpgas,

    -- * GpuDeviceInfo
    GpuDeviceInfo (..),
    newGpuDeviceInfo,
    gpuDeviceInfo_memoryInfo,
    gpuDeviceInfo_name,
    gpuDeviceInfo_count,
    gpuDeviceInfo_manufacturer,

    -- * GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (..),
    newGpuDeviceMemoryInfo,
    gpuDeviceMemoryInfo_sizeInMiB,

    -- * GpuInfo
    GpuInfo (..),
    newGpuInfo,
    gpuInfo_totalGpuMemoryInMiB,
    gpuInfo_gpus,

    -- * GroupIdentifier
    GroupIdentifier (..),
    newGroupIdentifier,
    groupIdentifier_groupName,
    groupIdentifier_groupId,

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
    historyRecord_eventType,
    historyRecord_eventInformation,
    historyRecord_timestamp,

    -- * HistoryRecordEntry
    HistoryRecordEntry (..),
    newHistoryRecordEntry,
    historyRecordEntry_eventType,
    historyRecordEntry_eventInformation,
    historyRecordEntry_timestamp,

    -- * Host
    Host (..),
    newHost,
    host_tags,
    host_instances,
    host_autoPlacement,
    host_clientToken,
    host_outpostArn,
    host_ownerId,
    host_availableCapacity,
    host_hostRecovery,
    host_hostId,
    host_allocationTime,
    host_state,
    host_availabilityZone,
    host_hostProperties,
    host_releaseTime,
    host_allowsMultipleInstanceTypes,
    host_memberOfServiceLinkedResourceGroup,
    host_hostReservationId,
    host_availabilityZoneId,

    -- * HostInstance
    HostInstance (..),
    newHostInstance,
    hostInstance_ownerId,
    hostInstance_instanceType,
    hostInstance_instanceId,

    -- * HostOffering
    HostOffering (..),
    newHostOffering,
    hostOffering_hourlyPrice,
    hostOffering_upfrontPrice,
    hostOffering_duration,
    hostOffering_currencyCode,
    hostOffering_instanceFamily,
    hostOffering_offeringId,
    hostOffering_paymentOption,

    -- * HostProperties
    HostProperties (..),
    newHostProperties,
    hostProperties_cores,
    hostProperties_sockets,
    hostProperties_instanceType,
    hostProperties_totalVCpus,
    hostProperties_instanceFamily,

    -- * HostReservation
    HostReservation (..),
    newHostReservation,
    hostReservation_tags,
    hostReservation_hourlyPrice,
    hostReservation_start,
    hostReservation_state,
    hostReservation_count,
    hostReservation_upfrontPrice,
    hostReservation_duration,
    hostReservation_currencyCode,
    hostReservation_end,
    hostReservation_instanceFamily,
    hostReservation_hostReservationId,
    hostReservation_offeringId,
    hostReservation_hostIdSet,
    hostReservation_paymentOption,

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
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_timestamp,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_associationId,

    -- * IamInstanceProfileSpecification
    IamInstanceProfileSpecification (..),
    newIamInstanceProfileSpecification,
    iamInstanceProfileSpecification_name,
    iamInstanceProfileSpecification_arn,

    -- * IcmpTypeCode
    IcmpTypeCode (..),
    newIcmpTypeCode,
    icmpTypeCode_type,
    icmpTypeCode_code,

    -- * IdFormat
    IdFormat (..),
    newIdFormat,
    idFormat_useLongIds,
    idFormat_deadline,
    idFormat_resource,

    -- * Image
    Image (..),
    newImage,
    image_tags,
    image_name,
    image_sriovNetSupport,
    image_blockDeviceMappings,
    image_creationDate,
    image_productCodes,
    image_platform,
    image_description,
    image_imageOwnerAlias,
    image_usageOperation,
    image_tpmSupport,
    image_ramdiskId,
    image_deprecationTime,
    image_platformDetails,
    image_imdsSupport,
    image_bootMode,
    image_kernelId,
    image_stateReason,
    image_enaSupport,
    image_rootDeviceName,
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
    imageDiskContainer_format,
    imageDiskContainer_userBucket,
    imageDiskContainer_deviceName,
    imageDiskContainer_snapshotId,
    imageDiskContainer_url,
    imageDiskContainer_description,

    -- * ImageRecycleBinInfo
    ImageRecycleBinInfo (..),
    newImageRecycleBinInfo,
    imageRecycleBinInfo_recycleBinEnterTime,
    imageRecycleBinInfo_name,
    imageRecycleBinInfo_description,
    imageRecycleBinInfo_recycleBinExitTime,
    imageRecycleBinInfo_imageId,

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
    importImageTask_tags,
    importImageTask_hypervisor,
    importImageTask_progress,
    importImageTask_licenseType,
    importImageTask_importTaskId,
    importImageTask_status,
    importImageTask_platform,
    importImageTask_description,
    importImageTask_usageOperation,
    importImageTask_encrypted,
    importImageTask_kmsKeyId,
    importImageTask_bootMode,
    importImageTask_licenseSpecifications,
    importImageTask_snapshotDetails,
    importImageTask_statusMessage,
    importImageTask_architecture,
    importImageTask_imageId,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    newImportInstanceLaunchSpecification,
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_architecture,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    newImportInstanceTaskDetails,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_description,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_instanceId,

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    newImportInstanceVolumeDetailItem,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_description,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_statusMessage,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    newImportSnapshotTask,
    importSnapshotTask_tags,
    importSnapshotTask_importTaskId,
    importSnapshotTask_description,
    importSnapshotTask_snapshotTaskDetail,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    newImportVolumeTaskDetails,
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_description,
    importVolumeTaskDetails_volume,
    importVolumeTaskDetails_image,

    -- * InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    newInferenceAcceleratorInfo,
    inferenceAcceleratorInfo_accelerators,

    -- * InferenceDeviceInfo
    InferenceDeviceInfo (..),
    newInferenceDeviceInfo,
    inferenceDeviceInfo_name,
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_manufacturer,

    -- * Instance
    Instance (..),
    newInstance,
    instance_ebsOptimized,
    instance_tags,
    instance_hibernationOptions,
    instance_iamInstanceProfile,
    instance_spotInstanceRequestId,
    instance_clientToken,
    instance_sriovNetSupport,
    instance_outpostArn,
    instance_blockDeviceMappings,
    instance_sourceDestCheck,
    instance_subnetId,
    instance_elasticGpuAssociations,
    instance_usageOperationUpdateTime,
    instance_productCodes,
    instance_capacityReservationSpecification,
    instance_capacityReservationId,
    instance_platform,
    instance_stateTransitionReason,
    instance_instanceLifecycle,
    instance_ipv6Address,
    instance_elasticInferenceAcceleratorAssociations,
    instance_usageOperation,
    instance_publicIpAddress,
    instance_publicDnsName,
    instance_securityGroups,
    instance_tpmSupport,
    instance_ramdiskId,
    instance_privateIpAddress,
    instance_maintenanceOptions,
    instance_privateDnsNameOptions,
    instance_platformDetails,
    instance_bootMode,
    instance_keyName,
    instance_privateDnsName,
    instance_kernelId,
    instance_vpcId,
    instance_cpuOptions,
    instance_stateReason,
    instance_enaSupport,
    instance_rootDeviceName,
    instance_networkInterfaces,
    instance_enclaveOptions,
    instance_licenses,
    instance_metadataOptions,
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
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,

    -- * InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (..),
    newInstanceBlockDeviceMappingSpecification,
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_deviceName,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    newInstanceCapacity,
    instanceCapacity_totalCapacity,
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,

    -- * InstanceCount
    InstanceCount (..),
    newInstanceCount,
    instanceCount_state,
    instanceCount_instanceCount,

    -- * InstanceCreditSpecification
    InstanceCreditSpecification (..),
    newInstanceCreditSpecification,
    instanceCreditSpecification_instanceId,
    instanceCreditSpecification_cpuCredits,

    -- * InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (..),
    newInstanceCreditSpecificationRequest,
    instanceCreditSpecificationRequest_instanceId,
    instanceCreditSpecificationRequest_cpuCredits,

    -- * InstanceEventWindow
    InstanceEventWindow (..),
    newInstanceEventWindow,
    instanceEventWindow_tags,
    instanceEventWindow_name,
    instanceEventWindow_state,
    instanceEventWindow_associationTarget,
    instanceEventWindow_timeRanges,
    instanceEventWindow_cronExpression,
    instanceEventWindow_instanceEventWindowId,

    -- * InstanceEventWindowAssociationRequest
    InstanceEventWindowAssociationRequest (..),
    newInstanceEventWindowAssociationRequest,
    instanceEventWindowAssociationRequest_instanceTags,
    instanceEventWindowAssociationRequest_dedicatedHostIds,
    instanceEventWindowAssociationRequest_instanceIds,

    -- * InstanceEventWindowAssociationTarget
    InstanceEventWindowAssociationTarget (..),
    newInstanceEventWindowAssociationTarget,
    instanceEventWindowAssociationTarget_tags,
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_instanceIds,

    -- * InstanceEventWindowDisassociationRequest
    InstanceEventWindowDisassociationRequest (..),
    newInstanceEventWindowDisassociationRequest,
    instanceEventWindowDisassociationRequest_instanceTags,
    instanceEventWindowDisassociationRequest_dedicatedHostIds,
    instanceEventWindowDisassociationRequest_instanceIds,

    -- * InstanceEventWindowStateChange
    InstanceEventWindowStateChange (..),
    newInstanceEventWindowStateChange,
    instanceEventWindowStateChange_state,
    instanceEventWindowStateChange_instanceEventWindowId,

    -- * InstanceEventWindowTimeRange
    InstanceEventWindowTimeRange (..),
    newInstanceEventWindowTimeRange,
    instanceEventWindowTimeRange_endHour,
    instanceEventWindowTimeRange_endWeekDay,
    instanceEventWindowTimeRange_startWeekDay,
    instanceEventWindowTimeRange_startHour,

    -- * InstanceEventWindowTimeRangeRequest
    InstanceEventWindowTimeRangeRequest (..),
    newInstanceEventWindowTimeRangeRequest,
    instanceEventWindowTimeRangeRequest_endHour,
    instanceEventWindowTimeRangeRequest_endWeekDay,
    instanceEventWindowTimeRangeRequest_startWeekDay,
    instanceEventWindowTimeRangeRequest_startHour,

    -- * InstanceExportDetails
    InstanceExportDetails (..),
    newInstanceExportDetails,
    instanceExportDetails_targetEnvironment,
    instanceExportDetails_instanceId,

    -- * InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    newInstanceFamilyCreditSpecification,
    instanceFamilyCreditSpecification_instanceFamily,
    instanceFamilyCreditSpecification_cpuCredits,

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
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_instanceMetadataTags,
    instanceMetadataOptionsRequest_httpProtocolIpv6,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    newInstanceMetadataOptionsResponse,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpTokens,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_instanceMetadataTags,
    instanceMetadataOptionsResponse_httpProtocolIpv6,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    newInstanceMonitoring,
    instanceMonitoring_monitoring,
    instanceMonitoring_instanceId,

    -- * InstanceNetworkInterface
    InstanceNetworkInterface (..),
    newInstanceNetworkInterface,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_status,
    instanceNetworkInterface_description,
    instanceNetworkInterface_association,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_groups,
    instanceNetworkInterface_ipv6Addresses,

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    newInstanceNetworkInterfaceAssociation,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_publicIp,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_customerOwnedIp,

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    newInstanceNetworkInterfaceAttachment,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    newInstanceNetworkInterfaceSpecification,
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_deviceIndex,

    -- * InstancePrivateIpAddress
    InstancePrivateIpAddress (..),
    newInstancePrivateIpAddress,
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateIpAddress,
    instancePrivateIpAddress_privateDnsName,

    -- * InstanceRequirements
    InstanceRequirements (..),
    newInstanceRequirements,
    instanceRequirements_instanceGenerations,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_bareMetal,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_localStorageTypes,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_acceleratorNames,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_vCpuCount,
    instanceRequirements_acceleratorCount,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_localStorage,
    instanceRequirements_memoryMiB,

    -- * InstanceRequirementsRequest
    InstanceRequirementsRequest (..),
    newInstanceRequirementsRequest,
    instanceRequirementsRequest_instanceGenerations,
    instanceRequirementsRequest_baselineEbsBandwidthMbps,
    instanceRequirementsRequest_bareMetal,
    instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_acceleratorTypes,
    instanceRequirementsRequest_totalLocalStorageGB,
    instanceRequirementsRequest_localStorageTypes,
    instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_allowedInstanceTypes,
    instanceRequirementsRequest_acceleratorNames,
    instanceRequirementsRequest_networkBandwidthGbps,
    instanceRequirementsRequest_acceleratorManufacturers,
    instanceRequirementsRequest_excludedInstanceTypes,
    instanceRequirementsRequest_networkInterfaceCount,
    instanceRequirementsRequest_requireHibernateSupport,
    instanceRequirementsRequest_acceleratorTotalMemoryMiB,
    instanceRequirementsRequest_acceleratorCount,
    instanceRequirementsRequest_burstablePerformance,
    instanceRequirementsRequest_cpuManufacturers,
    instanceRequirementsRequest_memoryGiBPerVCpu,
    instanceRequirementsRequest_localStorage,
    instanceRequirementsRequest_vCpuCount,
    instanceRequirementsRequest_memoryMiB,

    -- * InstanceRequirementsWithMetadataRequest
    InstanceRequirementsWithMetadataRequest (..),
    newInstanceRequirementsWithMetadataRequest,
    instanceRequirementsWithMetadataRequest_instanceRequirements,
    instanceRequirementsWithMetadataRequest_architectureTypes,
    instanceRequirementsWithMetadataRequest_virtualizationTypes,

    -- * InstanceSpecification
    InstanceSpecification (..),
    newInstanceSpecification,
    instanceSpecification_excludeDataVolumeIds,
    instanceSpecification_excludeBootVolume,
    instanceSpecification_instanceId,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_name,
    instanceState_code,

    -- * InstanceStateChange
    InstanceStateChange (..),
    newInstanceStateChange,
    instanceStateChange_previousState,
    instanceStateChange_instanceId,
    instanceStateChange_currentState,

    -- * InstanceStatus
    InstanceStatus (..),
    newInstanceStatus,
    instanceStatus_instanceState,
    instanceStatus_outpostArn,
    instanceStatus_instanceStatus,
    instanceStatus_availabilityZone,
    instanceStatus_systemStatus,
    instanceStatus_instanceId,
    instanceStatus_events,

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
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_description,
    instanceStatusEvent_notBefore,
    instanceStatusEvent_notBeforeDeadline,
    instanceStatusEvent_notAfter,

    -- * InstanceStatusSummary
    InstanceStatusSummary (..),
    newInstanceStatusSummary,
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- * InstanceStorageInfo
    InstanceStorageInfo (..),
    newInstanceStorageInfo,
    instanceStorageInfo_totalSizeInGB,
    instanceStorageInfo_disks,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_encryptionSupport,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    newInstanceTagNotificationAttribute,
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    newInstanceTypeInfo,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_supportedBootModes,

    -- * InstanceTypeInfoFromInstanceRequirements
    InstanceTypeInfoFromInstanceRequirements (..),
    newInstanceTypeInfoFromInstanceRequirements,
    instanceTypeInfoFromInstanceRequirements_instanceType,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    newInstanceTypeOffering,
    instanceTypeOffering_location,
    instanceTypeOffering_instanceType,
    instanceTypeOffering_locationType,

    -- * InstanceUsage
    InstanceUsage (..),
    newInstanceUsage,
    instanceUsage_usedInstanceCount,
    instanceUsage_accountId,

    -- * IntegrateServices
    IntegrateServices (..),
    newIntegrateServices,
    integrateServices_athenaIntegrations,

    -- * InternetGateway
    InternetGateway (..),
    newInternetGateway,
    internetGateway_tags,
    internetGateway_ownerId,
    internetGateway_attachments,
    internetGateway_internetGatewayId,

    -- * InternetGatewayAttachment
    InternetGatewayAttachment (..),
    newInternetGatewayAttachment,
    internetGatewayAttachment_state,
    internetGatewayAttachment_vpcId,

    -- * IpPermission
    IpPermission (..),
    newIpPermission,
    ipPermission_toPort,
    ipPermission_ipv6Ranges,
    ipPermission_prefixListIds,
    ipPermission_ipRanges,
    ipPermission_userIdGroupPairs,
    ipPermission_fromPort,
    ipPermission_ipProtocol,

    -- * IpRange
    IpRange (..),
    newIpRange,
    ipRange_description,
    ipRange_cidrIp,

    -- * Ipam
    Ipam (..),
    newIpam,
    ipam_tags,
    ipam_ownerId,
    ipam_ipamArn,
    ipam_scopeCount,
    ipam_state,
    ipam_description,
    ipam_operatingRegions,
    ipam_privateDefaultScopeId,
    ipam_ipamId,
    ipam_publicDefaultScopeId,
    ipam_ipamRegion,

    -- * IpamAddressHistoryRecord
    IpamAddressHistoryRecord (..),
    newIpamAddressHistoryRecord,
    ipamAddressHistoryRecord_resourceId,
    ipamAddressHistoryRecord_resourceType,
    ipamAddressHistoryRecord_resourceOverlapStatus,
    ipamAddressHistoryRecord_resourceOwnerId,
    ipamAddressHistoryRecord_resourceName,
    ipamAddressHistoryRecord_resourceCidr,
    ipamAddressHistoryRecord_sampledEndTime,
    ipamAddressHistoryRecord_resourceRegion,
    ipamAddressHistoryRecord_resourceComplianceStatus,
    ipamAddressHistoryRecord_vpcId,
    ipamAddressHistoryRecord_sampledStartTime,

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
    ipamPool_tags,
    ipamPool_ipamScopeArn,
    ipamPool_addressFamily,
    ipamPool_ipamScopeType,
    ipamPool_ownerId,
    ipamPool_allocationMaxNetmaskLength,
    ipamPool_publiclyAdvertisable,
    ipamPool_ipamArn,
    ipamPool_locale,
    ipamPool_state,
    ipamPool_sourceIpamPoolId,
    ipamPool_description,
    ipamPool_allocationResourceTags,
    ipamPool_allocationMinNetmaskLength,
    ipamPool_stateMessage,
    ipamPool_ipamPoolArn,
    ipamPool_poolDepth,
    ipamPool_ipamPoolId,
    ipamPool_allocationDefaultNetmaskLength,
    ipamPool_awsService,
    ipamPool_ipamRegion,
    ipamPool_autoImport,

    -- * IpamPoolAllocation
    IpamPoolAllocation (..),
    newIpamPoolAllocation,
    ipamPoolAllocation_resourceId,
    ipamPoolAllocation_resourceType,
    ipamPoolAllocation_cidr,
    ipamPoolAllocation_description,
    ipamPoolAllocation_resourceOwner,
    ipamPoolAllocation_resourceRegion,
    ipamPoolAllocation_ipamPoolAllocationId,

    -- * IpamPoolCidr
    IpamPoolCidr (..),
    newIpamPoolCidr,
    ipamPoolCidr_cidr,
    ipamPoolCidr_state,
    ipamPoolCidr_failureReason,

    -- * IpamPoolCidrFailureReason
    IpamPoolCidrFailureReason (..),
    newIpamPoolCidrFailureReason,
    ipamPoolCidrFailureReason_message,
    ipamPoolCidrFailureReason_code,

    -- * IpamResourceCidr
    IpamResourceCidr (..),
    newIpamResourceCidr,
    ipamResourceCidr_resourceId,
    ipamResourceCidr_resourceType,
    ipamResourceCidr_ipamScopeId,
    ipamResourceCidr_resourceOwnerId,
    ipamResourceCidr_overlapStatus,
    ipamResourceCidr_resourceName,
    ipamResourceCidr_resourceCidr,
    ipamResourceCidr_complianceStatus,
    ipamResourceCidr_resourceTags,
    ipamResourceCidr_resourceRegion,
    ipamResourceCidr_ipamPoolId,
    ipamResourceCidr_ipamId,
    ipamResourceCidr_vpcId,
    ipamResourceCidr_managementState,
    ipamResourceCidr_ipUsage,

    -- * IpamResourceTag
    IpamResourceTag (..),
    newIpamResourceTag,
    ipamResourceTag_key,
    ipamResourceTag_value,

    -- * IpamScope
    IpamScope (..),
    newIpamScope,
    ipamScope_tags,
    ipamScope_ipamScopeArn,
    ipamScope_ipamScopeId,
    ipamScope_ipamScopeType,
    ipamScope_ownerId,
    ipamScope_poolCount,
    ipamScope_ipamArn,
    ipamScope_state,
    ipamScope_description,
    ipamScope_isDefault,
    ipamScope_ipamRegion,

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
    ipv6Pool_tags,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_description,
    ipv6Pool_poolId,

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
    keyPairInfo_tags,
    keyPairInfo_publicKey,
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyType,
    keyPairInfo_keyName,
    keyPairInfo_createTime,
    keyPairInfo_keyPairId,

    -- * LastError
    LastError (..),
    newLastError,
    lastError_message,
    lastError_code,

    -- * LaunchPermission
    LaunchPermission (..),
    newLaunchPermission,
    launchPermission_organizationArn,
    launchPermission_userId,
    launchPermission_organizationalUnitArn,
    launchPermission_group,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    newLaunchPermissionModifications,
    launchPermissionModifications_remove,
    launchPermissionModifications_add,

    -- * LaunchSpecification
    LaunchSpecification (..),
    newLaunchSpecification,
    launchSpecification_ebsOptimized,
    launchSpecification_iamInstanceProfile,
    launchSpecification_placement,
    launchSpecification_userData,
    launchSpecification_blockDeviceMappings,
    launchSpecification_addressingType,
    launchSpecification_monitoring,
    launchSpecification_subnetId,
    launchSpecification_instanceType,
    launchSpecification_securityGroups,
    launchSpecification_ramdiskId,
    launchSpecification_keyName,
    launchSpecification_kernelId,
    launchSpecification_imageId,
    launchSpecification_networkInterfaces,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_tags,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_defaultVersionNumber,
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_launchTemplateName,

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    newLaunchTemplateAndOverridesResponse,
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    newLaunchTemplateBlockDeviceMapping,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_deviceName,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    newLaunchTemplateBlockDeviceMappingRequest,
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_deviceName,
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
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_iops,

    -- * LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (..),
    newLaunchTemplateEbsBlockDeviceRequest,
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_iops,

    -- * LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (..),
    newLaunchTemplateElasticInferenceAccelerator,
    launchTemplateElasticInferenceAccelerator_count,
    launchTemplateElasticInferenceAccelerator_type,

    -- * LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (..),
    newLaunchTemplateElasticInferenceAcceleratorResponse,
    launchTemplateElasticInferenceAcceleratorResponse_type,
    launchTemplateElasticInferenceAcceleratorResponse_count,

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
    launchTemplateIamInstanceProfileSpecification_name,
    launchTemplateIamInstanceProfileSpecification_arn,

    -- * LaunchTemplateIamInstanceProfileSpecificationRequest
    LaunchTemplateIamInstanceProfileSpecificationRequest (..),
    newLaunchTemplateIamInstanceProfileSpecificationRequest,
    launchTemplateIamInstanceProfileSpecificationRequest_name,
    launchTemplateIamInstanceProfileSpecificationRequest_arn,

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
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpTokens,
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_instanceMetadataTags,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    newLaunchTemplateInstanceMetadataOptionsRequest,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags,
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecification,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,

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
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_spotPrice,

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    newLaunchTemplatePlacement,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_groupId,

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    newLaunchTemplatePlacementRequest,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_groupId,

    -- * LaunchTemplatePrivateDnsNameOptions
    LaunchTemplatePrivateDnsNameOptions (..),
    newLaunchTemplatePrivateDnsNameOptions,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptions_hostnameType,

    -- * LaunchTemplatePrivateDnsNameOptionsRequest
    LaunchTemplatePrivateDnsNameOptionsRequest (..),
    newLaunchTemplatePrivateDnsNameOptionsRequest,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptionsRequest_hostnameType,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- * LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (..),
    newLaunchTemplateSpotMarketOptions,
    launchTemplateSpotMarketOptions_blockDurationMinutes,
    launchTemplateSpotMarketOptions_maxPrice,
    launchTemplateSpotMarketOptions_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_validUntil,

    -- * LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    newLaunchTemplateSpotMarketOptionsRequest,
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_maxPrice,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_validUntil,

    -- * LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (..),
    newLaunchTemplateTagSpecification,
    launchTemplateTagSpecification_tags,
    launchTemplateTagSpecification_resourceType,

    -- * LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (..),
    newLaunchTemplateTagSpecificationRequest,
    launchTemplateTagSpecificationRequest_tags,
    launchTemplateTagSpecificationRequest_resourceType,

    -- * LaunchTemplateVersion
    LaunchTemplateVersion (..),
    newLaunchTemplateVersion,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_launchTemplateName,

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
    loadBalancersConfig_targetGroupsConfig,
    loadBalancersConfig_classicLoadBalancersConfig,

    -- * LoadPermission
    LoadPermission (..),
    newLoadPermission,
    loadPermission_userId,
    loadPermission_group,

    -- * LoadPermissionModifications
    LoadPermissionModifications (..),
    newLoadPermissionModifications,
    loadPermissionModifications_remove,
    loadPermissionModifications_add,

    -- * LoadPermissionRequest
    LoadPermissionRequest (..),
    newLoadPermissionRequest,
    loadPermissionRequest_userId,
    loadPermissionRequest_group,

    -- * LocalGateway
    LocalGateway (..),
    newLocalGateway,
    localGateway_tags,
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_state,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    newLocalGatewayRoute,
    localGatewayRoute_type,
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_subnetId,
    localGatewayRoute_coipPoolId,
    localGatewayRoute_state,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_networkInterfaceId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_localGatewayRouteTableArn,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    newLocalGatewayRouteTable,
    localGatewayRouteTable_tags,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_state,
    localGatewayRouteTable_mode,
    localGatewayRouteTable_stateReason,
    localGatewayRouteTable_localGatewayRouteTableArn,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,

    -- * LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (..),
    newLocalGatewayRouteTableVpcAssociation,
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_vpcId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    newLocalGatewayVirtualInterface,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_vlan,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_localAddress,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    newLocalGatewayVirtualInterfaceGroup,
    localGatewayVirtualInterfaceGroup_tags,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    newManagedPrefixList,
    managedPrefixList_tags,
    managedPrefixList_addressFamily,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListId,
    managedPrefixList_state,
    managedPrefixList_maxEntries,
    managedPrefixList_prefixListArn,
    managedPrefixList_stateMessage,
    managedPrefixList_prefixListName,
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

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    newModifyTransitGatewayOptions,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_amazonSideAsn,
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,

    -- * ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (..),
    newModifyTransitGatewayVpcAttachmentRequestOptions,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,

    -- * ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (..),
    newModifyVpnTunnelOptionsSpecification,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_logOptions,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,

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
    natGateway_tags,
    natGateway_provisionedBandwidth,
    natGateway_failureCode,
    natGateway_deleteTime,
    natGateway_subnetId,
    natGateway_state,
    natGateway_natGatewayId,
    natGateway_failureMessage,
    natGateway_natGatewayAddresses,
    natGateway_connectivityType,
    natGateway_createTime,
    natGateway_vpcId,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    newNatGatewayAddress,
    natGatewayAddress_allocationId,
    natGatewayAddress_publicIp,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_privateIp,

    -- * NetworkAcl
    NetworkAcl (..),
    newNetworkAcl,
    networkAcl_tags,
    networkAcl_ownerId,
    networkAcl_networkAclId,
    networkAcl_associations,
    networkAcl_isDefault,
    networkAcl_entries,
    networkAcl_vpcId,

    -- * NetworkAclAssociation
    NetworkAclAssociation (..),
    newNetworkAclAssociation,
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclAssociationId,

    -- * NetworkAclEntry
    NetworkAclEntry (..),
    newNetworkAclEntry,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_egress,
    networkAclEntry_portRange,
    networkAclEntry_ruleNumber,
    networkAclEntry_cidrBlock,
    networkAclEntry_ruleAction,
    networkAclEntry_protocol,
    networkAclEntry_ipv6CidrBlock,

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
    networkCardInfo_networkCardIndex,
    networkCardInfo_networkPerformance,
    networkCardInfo_maximumNetworkInterfaces,

    -- * NetworkInfo
    NetworkInfo (..),
    newNetworkInfo,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_ipv6Supported,
    networkInfo_networkPerformance,
    networkInfo_efaInfo,
    networkInfo_defaultNetworkCardIndex,
    networkInfo_efaSupported,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_networkCards,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_encryptionInTransitSupported,
    networkInfo_enaSupport,
    networkInfo_maximumNetworkCards,

    -- * NetworkInsightsAccessScope
    NetworkInsightsAccessScope (..),
    newNetworkInsightsAccessScope,
    networkInsightsAccessScope_tags,
    networkInsightsAccessScope_networkInsightsAccessScopeId,
    networkInsightsAccessScope_networkInsightsAccessScopeArn,
    networkInsightsAccessScope_updatedDate,
    networkInsightsAccessScope_createdDate,

    -- * NetworkInsightsAccessScopeAnalysis
    NetworkInsightsAccessScopeAnalysis (..),
    newNetworkInsightsAccessScopeAnalysis,
    networkInsightsAccessScopeAnalysis_tags,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    networkInsightsAccessScopeAnalysis_analyzedEniCount,
    networkInsightsAccessScopeAnalysis_endDate,
    networkInsightsAccessScopeAnalysis_status,
    networkInsightsAccessScopeAnalysis_warningMessage,
    networkInsightsAccessScopeAnalysis_startDate,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn,
    networkInsightsAccessScopeAnalysis_findingsFound,
    networkInsightsAccessScopeAnalysis_statusMessage,

    -- * NetworkInsightsAccessScopeContent
    NetworkInsightsAccessScopeContent (..),
    newNetworkInsightsAccessScopeContent,
    networkInsightsAccessScopeContent_networkInsightsAccessScopeId,
    networkInsightsAccessScopeContent_excludePaths,
    networkInsightsAccessScopeContent_matchPaths,

    -- * NetworkInsightsAnalysis
    NetworkInsightsAnalysis (..),
    newNetworkInsightsAnalysis,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_warningMessage,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_networkInsightsPathId,

    -- * NetworkInsightsPath
    NetworkInsightsPath (..),
    newNetworkInsightsPath,
    networkInsightsPath_tags,
    networkInsightsPath_destination,
    networkInsightsPath_destinationIp,
    networkInsightsPath_sourceIp,
    networkInsightsPath_destinationPort,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_source,
    networkInsightsPath_createdDate,
    networkInsightsPath_protocol,
    networkInsightsPath_networkInsightsPathId,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_interfaceType,
    networkInterface_attachment,
    networkInterface_outpostArn,
    networkInterface_ownerId,
    networkInterface_ipv6Native,
    networkInterface_sourceDestCheck,
    networkInterface_privateIpAddresses,
    networkInterface_subnetId,
    networkInterface_requesterManaged,
    networkInterface_status,
    networkInterface_availabilityZone,
    networkInterface_description,
    networkInterface_association,
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_networkInterfaceId,
    networkInterface_denyAllIgwTraffic,
    networkInterface_ipv4Prefixes,
    networkInterface_privateIpAddress,
    networkInterface_privateDnsName,
    networkInterface_tagSet,
    networkInterface_ipv6Prefixes,
    networkInterface_vpcId,
    networkInterface_groups,
    networkInterface_ipv6Addresses,
    networkInterface_requesterId,

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    newNetworkInterfaceAssociation,
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_publicIp,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_associationId,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    newNetworkInterfaceAttachment,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_instanceOwnerId,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    newNetworkInterfaceAttachmentChanges,
    networkInterfaceAttachmentChanges_deleteOnTermination,
    networkInterfaceAttachmentChanges_attachmentId,

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
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_permission,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_permissionState,
    networkInterfacePermission_awsService,

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
    networkInterfacePrivateIpAddress_privateIpAddress,
    networkInterfacePrivateIpAddress_privateDnsName,

    -- * NewDhcpConfiguration
    NewDhcpConfiguration (..),
    newNewDhcpConfiguration,
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- * OnDemandOptions
    OnDemandOptions (..),
    newOnDemandOptions,
    onDemandOptions_singleInstanceType,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_allocationStrategy,
    onDemandOptions_maxTotalPrice,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    newOnDemandOptionsRequest,
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_maxTotalPrice,

    -- * PacketHeaderStatement
    PacketHeaderStatement (..),
    newPacketHeaderStatement,
    packetHeaderStatement_destinationPorts,
    packetHeaderStatement_destinationPrefixLists,
    packetHeaderStatement_protocols,
    packetHeaderStatement_destinationAddresses,
    packetHeaderStatement_sourceAddresses,
    packetHeaderStatement_sourcePorts,
    packetHeaderStatement_sourcePrefixLists,

    -- * PacketHeaderStatementRequest
    PacketHeaderStatementRequest (..),
    newPacketHeaderStatementRequest,
    packetHeaderStatementRequest_destinationPorts,
    packetHeaderStatementRequest_destinationPrefixLists,
    packetHeaderStatementRequest_protocols,
    packetHeaderStatementRequest_destinationAddresses,
    packetHeaderStatementRequest_sourceAddresses,
    packetHeaderStatementRequest_sourcePorts,
    packetHeaderStatementRequest_sourcePrefixLists,

    -- * PathComponent
    PathComponent (..),
    newPathComponent,
    pathComponent_routeTableRoute,
    pathComponent_vpc,
    pathComponent_additionalDetails,
    pathComponent_outboundHeader,
    pathComponent_attachedTo,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_explanations,
    pathComponent_transitGateway,
    pathComponent_sequenceNumber,
    pathComponent_destinationVpc,
    pathComponent_elasticLoadBalancerListener,
    pathComponent_subnet,
    pathComponent_inboundHeader,
    pathComponent_transitGatewayRouteTableRoute,
    pathComponent_component,
    pathComponent_securityGroupRule,

    -- * PathStatement
    PathStatement (..),
    newPathStatement,
    pathStatement_resourceStatement,
    pathStatement_packetHeaderStatement,

    -- * PathStatementRequest
    PathStatementRequest (..),
    newPathStatementRequest,
    pathStatementRequest_resourceStatement,
    pathStatementRequest_packetHeaderStatement,

    -- * PciId
    PciId (..),
    newPciId,
    pciId_deviceId,
    pciId_subsystemVendorId,
    pciId_subsystemId,
    pciId_vendorId,

    -- * PeeringAttachmentStatus
    PeeringAttachmentStatus (..),
    newPeeringAttachmentStatus,
    peeringAttachmentStatus_message,
    peeringAttachmentStatus_code,

    -- * PeeringConnectionOptions
    PeeringConnectionOptions (..),
    newPeeringConnectionOptions,
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- * PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    newPeeringConnectionOptionsRequest,
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- * PeeringTgwInfo
    PeeringTgwInfo (..),
    newPeeringTgwInfo,
    peeringTgwInfo_coreNetworkId,
    peeringTgwInfo_ownerId,
    peeringTgwInfo_transitGatewayId,
    peeringTgwInfo_region,

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
    placement_spreadDomain,
    placement_partitionNumber,
    placement_hostResourceGroupArn,
    placement_hostId,
    placement_availabilityZone,
    placement_groupName,
    placement_affinity,
    placement_tenancy,
    placement_groupId,

    -- * PlacementGroup
    PlacementGroup (..),
    newPlacementGroup,
    placementGroup_tags,
    placementGroup_partitionCount,
    placementGroup_spreadLevel,
    placementGroup_state,
    placementGroup_groupName,
    placementGroup_strategy,
    placementGroup_groupArn,
    placementGroup_groupId,

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
    prefixList_prefixListId,
    prefixList_cidrs,
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
    prefixListId_prefixListId,
    prefixListId_description,

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
    privateDnsNameConfiguration_type,
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_value,

    -- * PrivateDnsNameOptionsOnLaunch
    PrivateDnsNameOptionsOnLaunch (..),
    newPrivateDnsNameOptionsOnLaunch,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsOnLaunch_hostnameType,

    -- * PrivateDnsNameOptionsRequest
    PrivateDnsNameOptionsRequest (..),
    newPrivateDnsNameOptionsRequest,
    privateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsRequest_hostnameType,

    -- * PrivateDnsNameOptionsResponse
    PrivateDnsNameOptionsResponse (..),
    newPrivateDnsNameOptionsResponse,
    privateDnsNameOptionsResponse_enableResourceNameDnsARecord,
    privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsResponse_hostnameType,

    -- * PrivateIpAddressSpecification
    PrivateIpAddressSpecification (..),
    newPrivateIpAddressSpecification,
    privateIpAddressSpecification_primary,
    privateIpAddressSpecification_privateIpAddress,

    -- * ProcessorInfo
    ProcessorInfo (..),
    newProcessorInfo,
    processorInfo_sustainedClockSpeedInGhz,
    processorInfo_supportedArchitectures,

    -- * ProductCode
    ProductCode (..),
    newProductCode,
    productCode_productCodeType,
    productCode_productCodeId,

    -- * PropagatingVgw
    PropagatingVgw (..),
    newPropagatingVgw,
    propagatingVgw_gatewayId,

    -- * ProvisionedBandwidth
    ProvisionedBandwidth (..),
    newProvisionedBandwidth,
    provisionedBandwidth_requested,
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_status,
    provisionedBandwidth_provisioned,

    -- * PtrUpdateStatus
    PtrUpdateStatus (..),
    newPtrUpdateStatus,
    ptrUpdateStatus_status,
    ptrUpdateStatus_reason,
    ptrUpdateStatus_value,

    -- * PublicIpv4Pool
    PublicIpv4Pool (..),
    newPublicIpv4Pool,
    publicIpv4Pool_tags,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_description,
    publicIpv4Pool_poolId,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_poolAddressRanges,

    -- * PublicIpv4PoolRange
    PublicIpv4PoolRange (..),
    newPublicIpv4PoolRange,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_addressCount,

    -- * Purchase
    Purchase (..),
    newPurchase,
    purchase_hourlyPrice,
    purchase_upfrontPrice,
    purchase_duration,
    purchase_currencyCode,
    purchase_instanceFamily,
    purchase_hostReservationId,
    purchase_hostIdSet,
    purchase_paymentOption,

    -- * PurchaseRequest
    PurchaseRequest (..),
    newPurchaseRequest,
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_frequency,
    recurringCharge_amount,

    -- * ReferencedSecurityGroup
    ReferencedSecurityGroup (..),
    newReferencedSecurityGroup,
    referencedSecurityGroup_vpcPeeringConnectionId,
    referencedSecurityGroup_peeringStatus,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_groupId,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_optInStatus,
    regionInfo_regionName,
    regionInfo_endpoint,

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
    replaceRootVolumeTask_tags,
    replaceRootVolumeTask_taskState,
    replaceRootVolumeTask_snapshotId,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_deleteReplacedRootVolume,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_imageId,

    -- * RequestIpamResourceTag
    RequestIpamResourceTag (..),
    newRequestIpamResourceTag,
    requestIpamResourceTag_key,
    requestIpamResourceTag_value,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    newRequestLaunchTemplateData,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_instanceRequirements,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_maintenanceOptions,
    requestLaunchTemplateData_privateDnsNameOptions,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_disableApiStop,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_metadataOptions,

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    newRequestSpotLaunchSpecification,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_networkInterfaces,

    -- * Reservation
    Reservation (..),
    newReservation,
    reservation_instances,
    reservation_groups,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- * ReservationFleetInstanceSpecification
    ReservationFleetInstanceSpecification (..),
    newReservationFleetInstanceSpecification,
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_availabilityZone,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_weight,
    reservationFleetInstanceSpecification_availabilityZoneId,

    -- * ReservationValue
    ReservationValue (..),
    newReservationValue,
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,
    reservationValue_remainingUpfrontValue,

    -- * ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (..),
    newReservedInstanceLimitPrice,
    reservedInstanceLimitPrice_currencyCode,
    reservedInstanceLimitPrice_amount,

    -- * ReservedInstanceReservationValue
    ReservedInstanceReservationValue (..),
    newReservedInstanceReservationValue,
    reservedInstanceReservationValue_reservedInstanceId,
    reservedInstanceReservationValue_reservationValue,

    -- * ReservedInstances
    ReservedInstances (..),
    newReservedInstances,
    reservedInstances_tags,
    reservedInstances_offeringClass,
    reservedInstances_reservedInstancesId,
    reservedInstances_recurringCharges,
    reservedInstances_instanceTenancy,
    reservedInstances_start,
    reservedInstances_state,
    reservedInstances_offeringType,
    reservedInstances_availabilityZone,
    reservedInstances_instanceType,
    reservedInstances_duration,
    reservedInstances_currencyCode,
    reservedInstances_end,
    reservedInstances_scope,
    reservedInstances_instanceCount,
    reservedInstances_productDescription,
    reservedInstances_fixedPrice,
    reservedInstances_usagePrice,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    newReservedInstancesConfiguration,
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_scope,
    reservedInstancesConfiguration_instanceCount,

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    newReservedInstancesId,
    reservedInstancesId_reservedInstancesId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    newReservedInstancesListing,
    reservedInstancesListing_tags,
    reservedInstancesListing_clientToken,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_status,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_createDate,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_statusMessage,

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    newReservedInstancesModification,
    reservedInstancesModification_clientToken,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_status,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_createDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_reservedInstancesModificationId,

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    newReservedInstancesModificationResult,
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    newReservedInstancesOffering,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_reservedInstancesOfferingId,
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
    responseError_message,
    responseError_code,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    newResponseLaunchTemplateData,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_instanceRequirements,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_maintenanceOptions,
    responseLaunchTemplateData_privateDnsNameOptions,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_disableApiStop,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_metadataOptions,

    -- * Route
    Route (..),
    newRoute,
    route_localGatewayId,
    route_destinationPrefixListId,
    route_carrierGatewayId,
    route_transitGatewayId,
    route_state,
    route_natGatewayId,
    route_vpcPeeringConnectionId,
    route_destinationCidrBlock,
    route_coreNetworkArn,
    route_instanceId,
    route_egressOnlyInternetGatewayId,
    route_networkInterfaceId,
    route_gatewayId,
    route_destinationIpv6CidrBlock,
    route_origin,
    route_instanceOwnerId,

    -- * RouteTable
    RouteTable (..),
    newRouteTable,
    routeTable_tags,
    routeTable_propagatingVgws,
    routeTable_ownerId,
    routeTable_associations,
    routeTable_routeTableId,
    routeTable_vpcId,
    routeTable_routes,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    newRouteTableAssociation,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_associationState,
    routeTableAssociation_subnetId,
    routeTableAssociation_routeTableId,
    routeTableAssociation_gatewayId,
    routeTableAssociation_main,

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
    s3Storage_bucket,
    s3Storage_aWSAccessKeyId,
    s3Storage_uploadPolicy,
    s3Storage_uploadPolicySignature,
    s3Storage_prefix,

    -- * ScheduledInstance
    ScheduledInstance (..),
    newScheduledInstance,
    scheduledInstance_termStartDate,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_hourlyPrice,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_networkPlatform,
    scheduledInstance_platform,
    scheduledInstance_availabilityZone,
    scheduledInstance_instanceType,
    scheduledInstance_instanceCount,
    scheduledInstance_createDate,
    scheduledInstance_recurrence,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_termEndDate,
    scheduledInstance_slotDurationInHours,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    newScheduledInstanceAvailability,
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_slotDurationInHours,

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    newScheduledInstanceRecurrence,
    scheduledInstanceRecurrence_occurrenceDaySet,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    newScheduledInstanceRecurrenceRequest,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    newScheduledInstancesBlockDeviceMapping,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_deviceName,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,

    -- * ScheduledInstancesEbs
    ScheduledInstancesEbs (..),
    newScheduledInstancesEbs,
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_iops,

    -- * ScheduledInstancesIamInstanceProfile
    ScheduledInstancesIamInstanceProfile (..),
    newScheduledInstancesIamInstanceProfile,
    scheduledInstancesIamInstanceProfile_name,
    scheduledInstancesIamInstanceProfile_arn,

    -- * ScheduledInstancesIpv6Address
    ScheduledInstancesIpv6Address (..),
    newScheduledInstancesIpv6Address,
    scheduledInstancesIpv6Address_ipv6Address,

    -- * ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (..),
    newScheduledInstancesLaunchSpecification,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_networkInterfaces,
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
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_deviceIndex,

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
    securityGroup_tags,
    securityGroup_ipPermissionsEgress,
    securityGroup_ipPermissions,
    securityGroup_vpcId,
    securityGroup_ownerId,
    securityGroup_groupId,
    securityGroup_groupName,
    securityGroup_description,

    -- * SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    newSecurityGroupIdentifier,
    securityGroupIdentifier_groupName,
    securityGroupIdentifier_groupId,

    -- * SecurityGroupReference
    SecurityGroupReference (..),
    newSecurityGroupReference,
    securityGroupReference_vpcPeeringConnectionId,
    securityGroupReference_referencingVpcId,
    securityGroupReference_groupId,

    -- * SecurityGroupRule
    SecurityGroupRule (..),
    newSecurityGroupRule,
    securityGroupRule_tags,
    securityGroupRule_toPort,
    securityGroupRule_ipProtocol,
    securityGroupRule_cidrIpv6,
    securityGroupRule_prefixListId,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_isEgress,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_cidrIpv4,
    securityGroupRule_description,
    securityGroupRule_fromPort,
    securityGroupRule_groupId,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_securityGroupRuleId,
    securityGroupRuleDescription_description,

    -- * SecurityGroupRuleRequest
    SecurityGroupRuleRequest (..),
    newSecurityGroupRuleRequest,
    securityGroupRuleRequest_toPort,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_description,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_fromPort,

    -- * SecurityGroupRuleUpdate
    SecurityGroupRuleUpdate (..),
    newSecurityGroupRuleUpdate,
    securityGroupRuleUpdate_securityGroupRuleId,
    securityGroupRuleUpdate_securityGroupRule,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    newServiceConfiguration,
    serviceConfiguration_tags,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_supportedIpAddressTypes,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_serviceType,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_payerResponsibility,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceId,

    -- * ServiceDetail
    ServiceDetail (..),
    newServiceDetail,
    serviceDetail_tags,
    serviceDetail_acceptanceRequired,
    serviceDetail_supportedIpAddressTypes,
    serviceDetail_availabilityZones,
    serviceDetail_serviceType,
    serviceDetail_owner,
    serviceDetail_payerResponsibility,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_privateDnsNames,
    serviceDetail_privateDnsName,
    serviceDetail_serviceName,
    serviceDetail_serviceId,

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
    slotStartTimeRangeRequest_latestTime,
    slotStartTimeRangeRequest_earliestTime,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_tags,
    snapshot_ownerAlias,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_restoreExpiryTime,
    snapshot_kmsKeyId,
    snapshot_stateMessage,
    snapshot_storageTier,
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
    snapshotDetail_progress,
    snapshotDetail_format,
    snapshotDetail_userBucket,
    snapshotDetail_deviceName,
    snapshotDetail_snapshotId,
    snapshotDetail_url,
    snapshotDetail_status,
    snapshotDetail_description,
    snapshotDetail_statusMessage,
    snapshotDetail_diskImageSize,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    newSnapshotDiskContainer,
    snapshotDiskContainer_format,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_url,
    snapshotDiskContainer_description,

    -- * SnapshotInfo
    SnapshotInfo (..),
    newSnapshotInfo,
    snapshotInfo_tags,
    snapshotInfo_progress,
    snapshotInfo_outpostArn,
    snapshotInfo_ownerId,
    snapshotInfo_snapshotId,
    snapshotInfo_state,
    snapshotInfo_volumeSize,
    snapshotInfo_description,
    snapshotInfo_encrypted,
    snapshotInfo_volumeId,
    snapshotInfo_startTime,

    -- * SnapshotRecycleBinInfo
    SnapshotRecycleBinInfo (..),
    newSnapshotRecycleBinInfo,
    snapshotRecycleBinInfo_recycleBinEnterTime,
    snapshotRecycleBinInfo_snapshotId,
    snapshotRecycleBinInfo_description,
    snapshotRecycleBinInfo_recycleBinExitTime,
    snapshotRecycleBinInfo_volumeId,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    newSnapshotTaskDetail,
    snapshotTaskDetail_progress,
    snapshotTaskDetail_format,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_url,
    snapshotTaskDetail_status,
    snapshotTaskDetail_description,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_diskImageSize,

    -- * SnapshotTierStatus
    SnapshotTierStatus (..),
    newSnapshotTierStatus,
    snapshotTierStatus_tags,
    snapshotTierStatus_ownerId,
    snapshotTierStatus_lastTieringOperationStatusDetail,
    snapshotTierStatus_snapshotId,
    snapshotTierStatus_status,
    snapshotTierStatus_restoreExpiryTime,
    snapshotTierStatus_lastTieringOperationStatus,
    snapshotTierStatus_archivalCompleteTime,
    snapshotTierStatus_volumeId,
    snapshotTierStatus_lastTieringProgress,
    snapshotTierStatus_lastTieringStartTime,
    snapshotTierStatus_storageTier,

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    newSpotCapacityRebalance,
    spotCapacityRebalance_terminationDelay,
    spotCapacityRebalance_replacementStrategy,

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    newSpotDatafeedSubscription,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_prefix,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    newSpotFleetLaunchSpecification,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_instanceRequirements,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_weightedCapacity,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_networkInterfaces,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    newSpotFleetMonitoring,
    spotFleetMonitoring_enabled,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    newSpotFleetRequestConfig,
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_createTime,

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    newSpotFleetRequestConfigData,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_targetCapacityUnitType,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- * SpotFleetTagSpecification
    SpotFleetTagSpecification (..),
    newSpotFleetTagSpecification,
    spotFleetTagSpecification_tags,
    spotFleetTagSpecification_resourceType,

    -- * SpotInstanceRequest
    SpotInstanceRequest (..),
    newSpotInstanceRequest,
    spotInstanceRequest_tags,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_type,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_state,
    spotInstanceRequest_status,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_fault,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_createTime,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_spotPrice,

    -- * SpotInstanceStateFault
    SpotInstanceStateFault (..),
    newSpotInstanceStateFault,
    spotInstanceStateFault_message,
    spotInstanceStateFault_code,

    -- * SpotInstanceStatus
    SpotInstanceStatus (..),
    newSpotInstanceStatus,
    spotInstanceStatus_message,
    spotInstanceStatus_code,
    spotInstanceStatus_updateTime,

    -- * SpotMaintenanceStrategies
    SpotMaintenanceStrategies (..),
    newSpotMaintenanceStrategies,
    spotMaintenanceStrategies_capacityRebalance,

    -- * SpotMarketOptions
    SpotMarketOptions (..),
    newSpotMarketOptions,
    spotMarketOptions_blockDurationMinutes,
    spotMarketOptions_maxPrice,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_validUntil,

    -- * SpotOptions
    SpotOptions (..),
    newSpotOptions,
    spotOptions_singleInstanceType,
    spotOptions_instancePoolsToUseCount,
    spotOptions_singleAvailabilityZone,
    spotOptions_minTargetCapacity,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_maintenanceStrategies,
    spotOptions_allocationStrategy,
    spotOptions_maxTotalPrice,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    newSpotOptionsRequest,
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_maxTotalPrice,

    -- * SpotPlacement
    SpotPlacement (..),
    newSpotPlacement,
    spotPlacement_availabilityZone,
    spotPlacement_groupName,
    spotPlacement_tenancy,

    -- * SpotPlacementScore
    SpotPlacementScore (..),
    newSpotPlacementScore,
    spotPlacementScore_score,
    spotPlacementScore_region,
    spotPlacementScore_availabilityZoneId,

    -- * SpotPrice
    SpotPrice (..),
    newSpotPrice,
    spotPrice_timestamp,
    spotPrice_availabilityZone,
    spotPrice_instanceType,
    spotPrice_productDescription,
    spotPrice_spotPrice,

    -- * StaleIpPermission
    StaleIpPermission (..),
    newStaleIpPermission,
    staleIpPermission_toPort,
    staleIpPermission_ipProtocol,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipRanges,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_fromPort,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    newStaleSecurityGroup,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_groupName,
    staleSecurityGroup_description,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,
    staleSecurityGroup_groupId,

    -- * StateReason
    StateReason (..),
    newStateReason,
    stateReason_message,
    stateReason_code,

    -- * Storage
    Storage (..),
    newStorage,
    storage_s3,

    -- * StorageLocation
    StorageLocation (..),
    newStorageLocation,
    storageLocation_key,
    storageLocation_bucket,

    -- * StoreImageTaskResult
    StoreImageTaskResult (..),
    newStoreImageTaskResult,
    storeImageTaskResult_amiId,
    storeImageTaskResult_taskStartTime,
    storeImageTaskResult_bucket,
    storeImageTaskResult_storeTaskFailureReason,
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_progressPercentage,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_tags,
    subnet_outpostArn,
    subnet_ownerId,
    subnet_mapPublicIpOnLaunch,
    subnet_ipv6Native,
    subnet_enableLniAtDeviceIndex,
    subnet_defaultForAz,
    subnet_customerOwnedIpv4Pool,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_privateDnsNameOptionsOnLaunch,
    subnet_subnetArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_enableDns64,
    subnet_availabilityZoneId,
    subnet_availabilityZone,
    subnet_availableIpAddressCount,
    subnet_cidrBlock,
    subnet_state,
    subnet_subnetId,
    subnet_vpcId,

    -- * SubnetAssociation
    SubnetAssociation (..),
    newSubnetAssociation,
    subnetAssociation_subnetId,
    subnetAssociation_state,

    -- * SubnetCidrBlockState
    SubnetCidrBlockState (..),
    newSubnetCidrBlockState,
    subnetCidrBlockState_state,
    subnetCidrBlockState_statusMessage,

    -- * SubnetCidrReservation
    SubnetCidrReservation (..),
    newSubnetCidrReservation,
    subnetCidrReservation_tags,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_cidr,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_subnetId,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_description,

    -- * SubnetIpv6CidrBlockAssociation
    SubnetIpv6CidrBlockAssociation (..),
    newSubnetIpv6CidrBlockAssociation,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,

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
    tagSpecification_tags,
    tagSpecification_resourceType,

    -- * TargetCapacitySpecification
    TargetCapacitySpecification (..),
    newTargetCapacitySpecification,
    targetCapacitySpecification_targetCapacityUnitType,
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    newTargetCapacitySpecificationRequest,
    targetCapacitySpecificationRequest_targetCapacityUnitType,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_spotTargetCapacity,
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
    targetNetwork_status,
    targetNetwork_clientVpnEndpointId,
    targetNetwork_securityGroups,
    targetNetwork_vpcId,
    targetNetwork_associationId,
    targetNetwork_targetNetworkId,

    -- * TargetReservationValue
    TargetReservationValue (..),
    newTargetReservationValue,
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    newTerminateConnectionStatus,
    terminateConnectionStatus_previousStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_currentStatus,

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
    trafficMirrorFilter_tags,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_description,
    trafficMirrorFilter_networkServices,

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    newTrafficMirrorFilterRule,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_sourcePortRange,

    -- * TrafficMirrorPortRange
    TrafficMirrorPortRange (..),
    newTrafficMirrorPortRange,
    trafficMirrorPortRange_toPort,
    trafficMirrorPortRange_fromPort,

    -- * TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (..),
    newTrafficMirrorPortRangeRequest,
    trafficMirrorPortRangeRequest_toPort,
    trafficMirrorPortRangeRequest_fromPort,

    -- * TrafficMirrorSession
    TrafficMirrorSession (..),
    newTrafficMirrorSession,
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_description,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_virtualNetworkId,

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    newTrafficMirrorTarget,
    trafficMirrorTarget_tags,
    trafficMirrorTarget_type,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_description,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_gatewayLoadBalancerEndpointId,

    -- * TransitGateway
    TransitGateway (..),
    newTransitGateway,
    transitGateway_tags,
    transitGateway_ownerId,
    transitGateway_transitGatewayId,
    transitGateway_transitGatewayArn,
    transitGateway_state,
    transitGateway_description,
    transitGateway_options,
    transitGateway_creationTime,

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
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_state,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_association,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_transitGatewayOwnerId,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    newTransitGatewayAttachmentAssociation,
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- * TransitGatewayAttachmentBgpConfiguration
    TransitGatewayAttachmentBgpConfiguration (..),
    newTransitGatewayAttachmentBgpConfiguration,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,

    -- * TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    newTransitGatewayAttachmentPropagation,
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- * TransitGatewayConnect
    TransitGatewayConnect (..),
    newTransitGatewayConnect,
    transitGatewayConnect_tags,
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_state,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_options,
    transitGatewayConnect_creationTime,
    transitGatewayConnect_transportTransitGatewayAttachmentId,

    -- * TransitGatewayConnectOptions
    TransitGatewayConnectOptions (..),
    newTransitGatewayConnectOptions,
    transitGatewayConnectOptions_protocol,

    -- * TransitGatewayConnectPeer
    TransitGatewayConnectPeer (..),
    newTransitGatewayConnectPeer,
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,

    -- * TransitGatewayConnectPeerConfiguration
    TransitGatewayConnectPeerConfiguration (..),
    newTransitGatewayConnectPeerConfiguration,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_protocol,

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
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    newTransitGatewayMulticastDomainAssociation,
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociation_subnet,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    newTransitGatewayMulticastDomainAssociations,
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- * TransitGatewayMulticastDomainOptions
    TransitGatewayMulticastDomainOptions (..),
    newTransitGatewayMulticastDomainOptions,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    newTransitGatewayMulticastGroup,
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,

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
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_transitGatewayCidrBlocks,

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    newTransitGatewayPeeringAttachment,
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_options,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId,
    transitGatewayPeeringAttachment_requesterTgwInfo,

    -- * TransitGatewayPeeringAttachmentOptions
    TransitGatewayPeeringAttachmentOptions (..),
    newTransitGatewayPeeringAttachmentOptions,
    transitGatewayPeeringAttachmentOptions_dynamicRouting,

    -- * TransitGatewayPolicyRule
    TransitGatewayPolicyRule (..),
    newTransitGatewayPolicyRule,
    transitGatewayPolicyRule_metaData,
    transitGatewayPolicyRule_destinationCidrBlock,
    transitGatewayPolicyRule_destinationPortRange,
    transitGatewayPolicyRule_sourceCidrBlock,
    transitGatewayPolicyRule_protocol,
    transitGatewayPolicyRule_sourcePortRange,

    -- * TransitGatewayPolicyRuleMetaData
    TransitGatewayPolicyRuleMetaData (..),
    newTransitGatewayPolicyRuleMetaData,
    transitGatewayPolicyRuleMetaData_metaDataValue,
    transitGatewayPolicyRuleMetaData_metaDataKey,

    -- * TransitGatewayPolicyTable
    TransitGatewayPolicyTable (..),
    newTransitGatewayPolicyTable,
    transitGatewayPolicyTable_tags,
    transitGatewayPolicyTable_transitGatewayId,
    transitGatewayPolicyTable_transitGatewayPolicyTableId,
    transitGatewayPolicyTable_state,
    transitGatewayPolicyTable_creationTime,

    -- * TransitGatewayPolicyTableAssociation
    TransitGatewayPolicyTableAssociation (..),
    newTransitGatewayPolicyTableAssociation,
    transitGatewayPolicyTableAssociation_resourceId,
    transitGatewayPolicyTableAssociation_resourceType,
    transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId,
    transitGatewayPolicyTableAssociation_state,
    transitGatewayPolicyTableAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayPolicyTableEntry
    TransitGatewayPolicyTableEntry (..),
    newTransitGatewayPolicyTableEntry,
    transitGatewayPolicyTableEntry_policyRuleNumber,
    transitGatewayPolicyTableEntry_policyRule,
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
    transitGatewayPrefixListReference_transitGatewayAttachment,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_blackhole,
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
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    newTransitGatewayRoute,
    transitGatewayRoute_type,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_transitGatewayRouteTableAnnouncementId,

    -- * TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (..),
    newTransitGatewayRouteAttachment,
    transitGatewayRouteAttachment_resourceId,
    transitGatewayRouteAttachment_resourceType,
    transitGatewayRouteAttachment_transitGatewayAttachmentId,

    -- * TransitGatewayRouteTable
    TransitGatewayRouteTable (..),
    newTransitGatewayRouteTable,
    transitGatewayRouteTable_tags,
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_creationTime,
    transitGatewayRouteTable_transitGatewayRouteTableId,

    -- * TransitGatewayRouteTableAnnouncement
    TransitGatewayRouteTableAnnouncement (..),
    newTransitGatewayRouteTableAnnouncement,
    transitGatewayRouteTableAnnouncement_tags,
    transitGatewayRouteTableAnnouncement_peerCoreNetworkId,
    transitGatewayRouteTableAnnouncement_coreNetworkId,
    transitGatewayRouteTableAnnouncement_peerTransitGatewayId,
    transitGatewayRouteTableAnnouncement_transitGatewayId,
    transitGatewayRouteTableAnnouncement_state,
    transitGatewayRouteTableAnnouncement_announcementDirection,
    transitGatewayRouteTableAnnouncement_creationTime,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,
    transitGatewayRouteTableAnnouncement_peeringAttachmentId,

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
    transitGatewayRouteTableRoute_resourceId,
    transitGatewayRouteTableRoute_resourceType,
    transitGatewayRouteTableRoute_destinationCidr,
    transitGatewayRouteTableRoute_prefixListId,
    transitGatewayRouteTableRoute_state,
    transitGatewayRouteTableRoute_attachmentId,
    transitGatewayRouteTableRoute_routeOrigin,

    -- * TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (..),
    newTransitGatewayVpcAttachment,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_vpcOwnerId,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_subnetIds,

    -- * TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (..),
    newTransitGatewayVpcAttachmentOptions,
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,

    -- * TrunkInterfaceAssociation
    TrunkInterfaceAssociation (..),
    newTrunkInterfaceAssociation,
    trunkInterfaceAssociation_tags,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_vlanId,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_associationId,

    -- * TunnelOption
    TunnelOption (..),
    newTunnelOption,
    tunnelOption_tunnelInsideIpv6Cidr,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_ikeVersions,
    tunnelOption_preSharedKey,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_logOptions,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_startupAction,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_outsideIpAddress,
    tunnelOption_replayWindowSize,

    -- * UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    newUnsuccessfulInstanceCreditSpecificationItem,
    unsuccessfulInstanceCreditSpecificationItem_instanceId,
    unsuccessfulInstanceCreditSpecificationItem_error,

    -- * UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    newUnsuccessfulInstanceCreditSpecificationItemError,
    unsuccessfulInstanceCreditSpecificationItemError_message,
    unsuccessfulInstanceCreditSpecificationItemError_code,

    -- * UnsuccessfulItem
    UnsuccessfulItem (..),
    newUnsuccessfulItem,
    unsuccessfulItem_resourceId,
    unsuccessfulItem_error,

    -- * UnsuccessfulItemError
    UnsuccessfulItemError (..),
    newUnsuccessfulItemError,
    unsuccessfulItemError_message,
    unsuccessfulItemError_code,

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
    userIdGroupPair_vpcPeeringConnectionId,
    userIdGroupPair_groupName,
    userIdGroupPair_description,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_userId,
    userIdGroupPair_vpcId,
    userIdGroupPair_groupId,

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
    vCpuInfo_validThreadsPerCore,
    vCpuInfo_defaultCores,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validCores,
    vCpuInfo_defaultThreadsPerCore,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_message,
    validationError_code,

    -- * ValidationWarning
    ValidationWarning (..),
    newValidationWarning,
    validationWarning_errors,

    -- * VgwTelemetry
    VgwTelemetry (..),
    newVgwTelemetry,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_status,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_certificateArn,
    vgwTelemetry_statusMessage,
    vgwTelemetry_outsideIpAddress,

    -- * Volume
    Volume (..),
    newVolume,
    volume_tags,
    volume_outpostArn,
    volume_attachments,
    volume_kmsKeyId,
    volume_fastRestored,
    volume_throughput,
    volume_multiAttachEnabled,
    volume_iops,
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
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_state,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_volumeId,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_size,

    -- * VolumeModification
    VolumeModification (..),
    newVolumeModification,
    volumeModification_progress,
    volumeModification_targetThroughput,
    volumeModification_targetSize,
    volumeModification_originalVolumeType,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_endTime,
    volumeModification_originalIops,
    volumeModification_modificationState,
    volumeModification_originalThroughput,
    volumeModification_volumeId,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_targetIops,
    volumeModification_statusMessage,
    volumeModification_targetVolumeType,
    volumeModification_startTime,
    volumeModification_originalSize,

    -- * VolumeStatusAction
    VolumeStatusAction (..),
    newVolumeStatusAction,
    volumeStatusAction_eventType,
    volumeStatusAction_code,
    volumeStatusAction_description,
    volumeStatusAction_eventId,

    -- * VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    newVolumeStatusAttachmentStatus,
    volumeStatusAttachmentStatus_ioPerformance,
    volumeStatusAttachmentStatus_instanceId,

    -- * VolumeStatusDetails
    VolumeStatusDetails (..),
    newVolumeStatusDetails,
    volumeStatusDetails_name,
    volumeStatusDetails_status,

    -- * VolumeStatusEvent
    VolumeStatusEvent (..),
    newVolumeStatusEvent,
    volumeStatusEvent_eventType,
    volumeStatusEvent_description,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventId,
    volumeStatusEvent_notAfter,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    newVolumeStatusInfo,
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    newVolumeStatusItem,
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeStatus,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_attachmentStatuses,
    volumeStatusItem_events,
    volumeStatusItem_volumeId,
    volumeStatusItem_actions,

    -- * Vpc
    Vpc (..),
    newVpc,
    vpc_tags,
    vpc_ownerId,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_isDefault,
    vpc_cidrBlockAssociationSet,
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
    vpcCidrBlockAssociation_cidrBlockState,
    vpcCidrBlockAssociation_cidrBlock,
    vpcCidrBlockAssociation_associationId,

    -- * VpcCidrBlockState
    VpcCidrBlockState (..),
    newVpcCidrBlockState,
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- * VpcClassicLink
    VpcClassicLink (..),
    newVpcClassicLink,
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_vpcId,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_tags,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_ownerId,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_state,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_lastError,
    vpcEndpoint_policyDocument,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcId,
    vpcEndpoint_serviceName,
    vpcEndpoint_dnsOptions,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_ipAddressType,
    vpcEndpoint_subnetIds,
    vpcEndpoint_groups,

    -- * VpcEndpointConnection
    VpcEndpointConnection (..),
    newVpcEndpointConnection,
    vpcEndpointConnection_tags,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_ipAddressType,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_vpcEndpointConnectionId,

    -- * VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (..),
    newVpcIpv6CidrBlockAssociation,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_tags,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,

    -- * VpcPeeringConnectionOptionsDescription
    VpcPeeringConnectionOptionsDescription (..),
    newVpcPeeringConnectionOptionsDescription,
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- * VpcPeeringConnectionStateReason
    VpcPeeringConnectionStateReason (..),
    newVpcPeeringConnectionStateReason,
    vpcPeeringConnectionStateReason_message,
    vpcPeeringConnectionStateReason_code,

    -- * VpcPeeringConnectionVpcInfo
    VpcPeeringConnectionVpcInfo (..),
    newVpcPeeringConnectionVpcInfo,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_vpcId,

    -- * VpnConnection
    VpnConnection (..),
    newVpnConnection,
    vpnConnection_tags,
    vpnConnection_transitGatewayId,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_gatewayAssociationState,
    vpnConnection_coreNetworkAttachmentArn,
    vpnConnection_options,
    vpnConnection_coreNetworkArn,
    vpnConnection_vpnGatewayId,
    vpnConnection_category,
    vpnConnection_vgwTelemetry,
    vpnConnection_routes,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- * VpnConnectionDeviceType
    VpnConnectionDeviceType (..),
    newVpnConnectionDeviceType,
    vpnConnectionDeviceType_software,
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,
    vpnConnectionDeviceType_vendor,

    -- * VpnConnectionOptions
    VpnConnectionOptions (..),
    newVpnConnectionOptions,
    vpnConnectionOptions_outsideIpAddressType,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_transportTransitGatewayAttachmentId,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv6NetworkCidr,

    -- * VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (..),
    newVpnConnectionOptionsSpecification,
    vpnConnectionOptionsSpecification_outsideIpAddressType,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,

    -- * VpnGateway
    VpnGateway (..),
    newVpnGateway,
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_state,
    vpnGateway_availabilityZone,
    vpnGateway_vpnGatewayId,
    vpnGateway_vpcAttachments,
    vpnGateway_amazonSideAsn,

    -- * VpnStaticRoute
    VpnStaticRoute (..),
    newVpnStaticRoute,
    vpnStaticRoute_state,
    vpnStaticRoute_destinationCidrBlock,
    vpnStaticRoute_source,

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
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_logOptions,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_replayWindowSize,
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
import Amazonka.EC2.Types.CreateVolumePermission
import Amazonka.EC2.Types.CreateVolumePermissionModifications
import Amazonka.EC2.Types.CreditSpecification
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.CustomerGateway
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
import Amazonka.EC2.Types.ModifyAvailabilityZoneOptInStatus
import Amazonka.EC2.Types.ModifyTransitGatewayOptions
import Amazonka.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
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
import Amazonka.EC2.Types.VCpuCountRange
import Amazonka.EC2.Types.VCpuCountRangeRequest
import Amazonka.EC2.Types.VCpuInfo
import Amazonka.EC2.Types.ValidationError
import Amazonka.EC2.Types.ValidationWarning
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "RequestLimitExceeded"
              Prelude.. Core.hasStatus 503
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "EC2ThrottledException"
              Prelude.. Core.hasStatus 503
          )
          e =
        Prelude.Just "ec2_throttled_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing
