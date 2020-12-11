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
    ec2Service,

    -- * Errors

    -- * AccountAttributeName
    AccountAttributeName (..),

    -- * ActivityStatus
    ActivityStatus (..),

    -- * AddressStatus
    AddressStatus (..),

    -- * Affinity
    Affinity (..),

    -- * AllocationState
    AllocationState (..),

    -- * AllocationStrategy
    AllocationStrategy (..),

    -- * AllowsMultipleInstanceTypes
    AllowsMultipleInstanceTypes (..),

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

    -- * AutoAcceptSharedAttachmentsValue
    AutoAcceptSharedAttachmentsValue (..),

    -- * AutoPlacement
    AutoPlacement (..),

    -- * AvailabilityZoneOptInStatus
    AvailabilityZoneOptInStatus (..),

    -- * AvailabilityZoneState
    AvailabilityZoneState (..),

    -- * BatchState
    BatchState (..),

    -- * BundleTaskState
    BundleTaskState (..),

    -- * ByoipCidrState
    ByoipCidrState (..),

    -- * CancelBatchErrorCode
    CancelBatchErrorCode (..),

    -- * CancelSpotInstanceRequestState
    CancelSpotInstanceRequestState (..),

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

    -- * ClientVPNAuthenticationType
    ClientVPNAuthenticationType (..),

    -- * ClientVPNAuthorizationRuleStatusCode
    ClientVPNAuthorizationRuleStatusCode (..),

    -- * ClientVPNConnectionStatusCode
    ClientVPNConnectionStatusCode (..),

    -- * ClientVPNEndpointAttributeStatusCode
    ClientVPNEndpointAttributeStatusCode (..),

    -- * ClientVPNEndpointStatusCode
    ClientVPNEndpointStatusCode (..),

    -- * ClientVPNRouteStatusCode
    ClientVPNRouteStatusCode (..),

    -- * ConnectionNotificationState
    ConnectionNotificationState (..),

    -- * ConnectionNotificationType
    ConnectionNotificationType (..),

    -- * ContainerFormat
    ContainerFormat (..),

    -- * ConversionTaskState
    ConversionTaskState (..),

    -- * CopyTagsFromSource
    CopyTagsFromSource (..),

    -- * CurrencyCodeValues
    CurrencyCodeValues (..),

    -- * DNSNameState
    DNSNameState (..),

    -- * DNSSupportValue
    DNSSupportValue (..),

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

    -- * DeviceType
    DeviceType (..),

    -- * DiskImageFormat
    DiskImageFormat (..),

    -- * DiskType
    DiskType (..),

    -- * DomainType
    DomainType (..),

    -- * EBSEncryptionSupport
    EBSEncryptionSupport (..),

    -- * EBSNvmeSupport
    EBSNvmeSupport (..),

    -- * EBSOptimizedSupport
    EBSOptimizedSupport (..),

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

    -- * FastSnapshotRestoreStateCode
    FastSnapshotRestoreStateCode (..),

    -- * FleetActivityStatus
    FleetActivityStatus (..),

    -- * FleetCapacityReservationUsageStrategy
    FleetCapacityReservationUsageStrategy (..),

    -- * FleetEventType
    FleetEventType (..),

    -- * FleetExcessCapacityTerminationPolicy
    FleetExcessCapacityTerminationPolicy (..),

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

    -- * GatewayType
    GatewayType (..),

    -- * HTTPTokensState
    HTTPTokensState (..),

    -- * HostRecovery
    HostRecovery (..),

    -- * HostTenancy
    HostTenancy (..),

    -- * HypervisorType
    HypervisorType (..),

    -- * IAMInstanceProfileAssociationState
    IAMInstanceProfileAssociationState (..),

    -- * IPv6SupportValue
    IPv6SupportValue (..),

    -- * ImageAttributeName
    ImageAttributeName (..),

    -- * ImageState
    ImageState (..),

    -- * ImageTypeValues
    ImageTypeValues (..),

    -- * InstanceAttributeName
    InstanceAttributeName (..),

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

    -- * InstanceStateName
    InstanceStateName (..),

    -- * InstanceType
    InstanceType (..),

    -- * InstanceTypeHypervisor
    InstanceTypeHypervisor (..),

    -- * InterfacePermissionType
    InterfacePermissionType (..),

    -- * LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- * LaunchTemplateHTTPTokensState
    LaunchTemplateHTTPTokensState (..),

    -- * LaunchTemplateInstanceMetadataEndpointState
    LaunchTemplateInstanceMetadataEndpointState (..),

    -- * LaunchTemplateInstanceMetadataOptionsState
    LaunchTemplateInstanceMetadataOptionsState (..),

    -- * ListingState
    ListingState (..),

    -- * ListingStatus
    ListingStatus (..),

    -- * LocalGatewayRouteState
    LocalGatewayRouteState (..),

    -- * LocalGatewayRouteType
    LocalGatewayRouteType (..),

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

    -- * RIProductDescription
    RIProductDescription (..),

    -- * RecurringChargeFrequency
    RecurringChargeFrequency (..),

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

    -- * State
    State (..),

    -- * StatusName
    StatusName (..),

    -- * StatusType
    StatusType (..),

    -- * SubnetCidrBlockStateCode
    SubnetCidrBlockStateCode (..),

    -- * SubnetState
    SubnetState (..),

    -- * SummaryStatus
    SummaryStatus (..),

    -- * TelemetryStatus
    TelemetryStatus (..),

    -- * Tenancy
    Tenancy (..),

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

    -- * TransitGatewayMulitcastDomainAssociationState
    TransitGatewayMulitcastDomainAssociationState (..),

    -- * TransitGatewayMulticastDomainState
    TransitGatewayMulticastDomainState (..),

    -- * TransitGatewayPrefixListReferenceState
    TransitGatewayPrefixListReferenceState (..),

    -- * TransitGatewayPropagationState
    TransitGatewayPropagationState (..),

    -- * TransitGatewayRouteState
    TransitGatewayRouteState (..),

    -- * TransitGatewayRouteTableState
    TransitGatewayRouteTableState (..),

    -- * TransitGatewayRouteType
    TransitGatewayRouteType (..),

    -- * TransitGatewayState
    TransitGatewayState (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * TunnelInsideIPVersion
    TunnelInsideIPVersion (..),

    -- * UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- * UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- * UsageClassType
    UsageClassType (..),

    -- * VPCAttributeName
    VPCAttributeName (..),

    -- * VPCCidrBlockStateCode
    VPCCidrBlockStateCode (..),

    -- * VPCEndpointType
    VPCEndpointType (..),

    -- * VPCPeeringConnectionStateReasonCode
    VPCPeeringConnectionStateReasonCode (..),

    -- * VPCState
    VPCState (..),

    -- * VPCTenancy
    VPCTenancy (..),

    -- * VPNEcmpSupportValue
    VPNEcmpSupportValue (..),

    -- * VPNProtocol
    VPNProtocol (..),

    -- * VPNState
    VPNState (..),

    -- * VPNStaticRouteSource
    VPNStaticRouteSource (..),

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

    -- * AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeValues,
    aaAttributeName,

    -- * AccountAttributeValue
    AccountAttributeValue (..),
    mkAccountAttributeValue,
    aavAttributeValue,

    -- * ActiveInstance
    ActiveInstance (..),
    mkActiveInstance,
    aiInstanceId,
    aiInstanceHealth,
    aiInstanceType,
    aiSpotInstanceRequestId,

    -- * AddPrefixListEntry
    AddPrefixListEntry (..),
    mkAddPrefixListEntry,
    apleDescription,
    apleCidr,

    -- * Address
    Address (..),
    mkAddress,
    aAssociationId,
    aInstanceId,
    aNetworkInterfaceOwnerId,
    aAllocationId,
    aCarrierIP,
    aNetworkBorderGroup,
    aDomain,
    aNetworkInterfaceId,
    aPublicIPv4Pool,
    aCustomerOwnedIPv4Pool,
    aCustomerOwnedIP,
    aPrivateIPAddress,
    aPublicIP,
    aTags,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    mkAllowedPrincipal,
    apPrincipalType,
    apPrincipal,

    -- * AssignedPrivateIPAddress
    AssignedPrivateIPAddress (..),
    mkAssignedPrivateIPAddress,
    apiaPrivateIPAddress,

    -- * AssociatedRole
    AssociatedRole (..),
    mkAssociatedRole,
    arCertificateS3BucketName,
    arCertificateS3ObjectKey,
    arEncryptionKMSKeyId,
    arAssociatedRoleARN,

    -- * AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    mkAssociatedTargetNetwork,
    atnNetworkId,
    atnNetworkType,

    -- * AssociationStatus
    AssociationStatus (..),
    mkAssociationStatus,
    asCode,
    asMessage,

    -- * AttributeBooleanValue
    AttributeBooleanValue (..),
    mkAttributeBooleanValue,
    abvValue,

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avValue,

    -- * AuthorizationRule
    AuthorizationRule (..),
    mkAuthorizationRule,
    arStatus,
    arAccessAll,
    arClientVPNEndpointId,
    arGroupId,
    arDestinationCidr,
    arDescription,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azState,
    azParentZoneId,
    azRegionName,
    azParentZoneName,
    azNetworkBorderGroup,
    azZoneId,
    azZoneName,
    azOptInStatus,
    azMessages,
    azGroupName,
    azZoneType,

    -- * AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    mkAvailabilityZoneMessage,
    azmMessage,

    -- * AvailableCapacity
    AvailableCapacity (..),
    mkAvailableCapacity,
    acAvailableInstanceCapacity,
    acAvailableVCPUs,

    -- * BlobAttributeValue
    BlobAttributeValue (..),
    mkBlobAttributeValue,
    bavValue,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- * BundleTask
    BundleTask (..),
    mkBundleTask,
    btBundleTaskError,
    btBundleId,
    btInstanceId,
    btProgress,
    btStartTime,
    btState,
    btStorage,
    btUpdateTime,

    -- * BundleTaskError
    BundleTaskError (..),
    mkBundleTaskError,
    bteCode,
    bteMessage,

    -- * ByoipCidr
    ByoipCidr (..),
    mkByoipCidr,
    bcState,
    bcCidr,
    bcStatusMessage,
    bcDescription,

    -- * CPUOptions
    CPUOptions (..),
    mkCPUOptions,
    coCoreCount,
    coThreadsPerCore,

    -- * CPUOptionsRequest
    CPUOptionsRequest (..),
    mkCPUOptionsRequest,
    corCoreCount,
    corThreadsPerCore,

    -- * CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    mkCancelSpotFleetRequestsError,
    csfreCode,
    csfreMessage,

    -- * CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (..),
    mkCancelSpotFleetRequestsErrorItem,
    csfreiError,
    csfreiSpotFleetRequestId,

    -- * CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (..),
    mkCancelSpotFleetRequestsSuccessItem,
    csfrsiCurrentSpotFleetRequestState,
    csfrsiSpotFleetRequestId,
    csfrsiPreviousSpotFleetRequestState,

    -- * CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    mkCancelledSpotInstanceRequest,
    csirState,
    csirSpotInstanceRequestId,

    -- * CapacityReservation
    CapacityReservation (..),
    mkCapacityReservation,
    crState,
    crAvailabilityZoneId,
    crCreateDate,
    crEndDate,
    crAvailableInstanceCount,
    crEphemeralStorage,
    crInstancePlatform,
    crInstanceMatchCriteria,
    crCapacityReservationId,
    crInstanceType,
    crEBSOptimized,
    crOwnerId,
    crAvailabilityZone,
    crTenancy,
    crTotalInstanceCount,
    crEndDateType,
    crTags,
    crCapacityReservationARN,

    -- * CapacityReservationGroup
    CapacityReservationGroup (..),
    mkCapacityReservationGroup,
    crgOwnerId,
    crgGroupARN,

    -- * CapacityReservationOptions
    CapacityReservationOptions (..),
    mkCapacityReservationOptions,
    croUsageStrategy,

    -- * CapacityReservationOptionsRequest
    CapacityReservationOptionsRequest (..),
    mkCapacityReservationOptionsRequest,
    crorUsageStrategy,

    -- * CapacityReservationSpecification
    CapacityReservationSpecification (..),
    mkCapacityReservationSpecification,
    cCapacityReservationTarget,
    cCapacityReservationPreference,

    -- * CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (..),
    mkCapacityReservationSpecificationResponse,
    crsCapacityReservationTarget,
    crsCapacityReservationPreference,

    -- * CapacityReservationTarget
    CapacityReservationTarget (..),
    mkCapacityReservationTarget,
    crtCapacityReservationId,
    crtCapacityReservationResourceGroupARN,

    -- * CapacityReservationTargetResponse
    CapacityReservationTargetResponse (..),
    mkCapacityReservationTargetResponse,
    cCapacityReservationId,
    cCapacityReservationResourceGroupARN,

    -- * CarrierGateway
    CarrierGateway (..),
    mkCarrierGateway,
    cgState,
    cgVPCId,
    cgOwnerId,
    cgTags,
    cgCarrierGatewayId,

    -- * CertificateAuthentication
    CertificateAuthentication (..),
    mkCertificateAuthentication,
    caClientRootCertificateChain,

    -- * CertificateAuthenticationRequest
    CertificateAuthenticationRequest (..),
    mkCertificateAuthenticationRequest,
    carClientRootCertificateChainARN,

    -- * CidrAuthorizationContext
    CidrAuthorizationContext (..),
    mkCidrAuthorizationContext,
    cacMessage,
    cacSignature,

    -- * CidrBlock
    CidrBlock (..),
    mkCidrBlock,
    cbCidrBlock,

    -- * ClassicLinkDNSSupport
    ClassicLinkDNSSupport (..),
    mkClassicLinkDNSSupport,
    cldsVPCId,
    cldsClassicLinkDNSSupported,

    -- * ClassicLinkInstance
    ClassicLinkInstance (..),
    mkClassicLinkInstance,
    cliInstanceId,
    cliGroups,
    cliVPCId,
    cliTags,

    -- * ClassicLoadBalancer
    ClassicLoadBalancer (..),
    mkClassicLoadBalancer,
    clbName,

    -- * ClassicLoadBalancersConfig
    ClassicLoadBalancersConfig (..),
    mkClassicLoadBalancersConfig,
    clbcClassicLoadBalancers,

    -- * ClientCertificateRevocationListStatus
    ClientCertificateRevocationListStatus (..),
    mkClientCertificateRevocationListStatus,
    ccrlsCode,
    ccrlsMessage,

    -- * ClientConnectOptions
    ClientConnectOptions (..),
    mkClientConnectOptions,
    ccoEnabled,
    ccoLambdaFunctionARN,

    -- * ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    mkClientConnectResponseOptions,
    ccroStatus,
    ccroEnabled,
    ccroLambdaFunctionARN,

    -- * ClientData
    ClientData (..),
    mkClientData,
    cdUploadStart,
    cdUploadSize,
    cdUploadEnd,
    cdComment,

    -- * ClientVPNAuthentication
    ClientVPNAuthentication (..),
    mkClientVPNAuthentication,
    cvaActiveDirectory,
    cvaFederatedAuthentication,
    cvaMutualAuthentication,
    cvaType,

    -- * ClientVPNAuthenticationRequest
    ClientVPNAuthenticationRequest (..),
    mkClientVPNAuthenticationRequest,
    cvarActiveDirectory,
    cvarFederatedAuthentication,
    cvarMutualAuthentication,
    cvarType,

    -- * ClientVPNAuthorizationRuleStatus
    ClientVPNAuthorizationRuleStatus (..),
    mkClientVPNAuthorizationRuleStatus,
    cvarsCode,
    cvarsMessage,

    -- * ClientVPNConnection
    ClientVPNConnection (..),
    mkClientVPNConnection,
    cvcIngressPackets,
    cvcStatus,
    cvcConnectionEndTime,
    cvcCommonName,
    cvcPostureComplianceStatuses,
    cvcConnectionEstablishedTime,
    cvcConnectionId,
    cvcIngressBytes,
    cvcUsername,
    cvcEgressBytes,
    cvcClientVPNEndpointId,
    cvcClientIP,
    cvcEgressPackets,
    cvcTimestamp,

    -- * ClientVPNConnectionStatus
    ClientVPNConnectionStatus (..),
    mkClientVPNConnectionStatus,
    cvcsCode,
    cvcsMessage,

    -- * ClientVPNEndpoint
    ClientVPNEndpoint (..),
    mkClientVPNEndpoint,
    cveCreationTime,
    cveStatus,
    cveAssociatedTargetNetworks,
    cveSecurityGroupIds,
    cveConnectionLogOptions,
    cveSplitTunnel,
    cveTransportProtocol,
    cveVPCId,
    cveVPNPort,
    cveDeletionTime,
    cveClientCidrBlock,
    cveDNSServers,
    cveClientVPNEndpointId,
    cveClientConnectOptions,
    cveServerCertificateARN,
    cveAuthenticationOptions,
    cveSelfServicePortalURL,
    cveDescription,
    cveDNSName,
    cveVPNProtocol,
    cveTags,

    -- * ClientVPNEndpointAttributeStatus
    ClientVPNEndpointAttributeStatus (..),
    mkClientVPNEndpointAttributeStatus,
    cveasCode,
    cveasMessage,

    -- * ClientVPNEndpointStatus
    ClientVPNEndpointStatus (..),
    mkClientVPNEndpointStatus,
    cvesCode,
    cvesMessage,

    -- * ClientVPNRoute
    ClientVPNRoute (..),
    mkClientVPNRoute,
    cvrStatus,
    cvrOrigin,
    cvrClientVPNEndpointId,
    cvrTargetSubnet,
    cvrDestinationCidr,
    cvrType,
    cvrDescription,

    -- * ClientVPNRouteStatus
    ClientVPNRouteStatus (..),
    mkClientVPNRouteStatus,
    cvrsCode,
    cvrsMessage,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    mkCoipAddressUsage,
    cauAllocationId,
    cauAWSAccountId,
    cauCoIP,
    cauAWSService,

    -- * CoipPool
    CoipPool (..),
    mkCoipPool,
    cpPoolId,
    cpLocalGatewayRouteTableId,
    cpPoolCidrs,
    cpTags,
    cpPoolARN,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    mkConnectionLogOptions,
    cloEnabled,
    cloCloudwatchLogStream,
    cloCloudwatchLogGroup,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    mkConnectionLogResponseOptions,
    clroEnabled,
    clroCloudwatchLogStream,
    clroCloudwatchLogGroup,

    -- * ConnectionNotification
    ConnectionNotification (..),
    mkConnectionNotification,
    cnConnectionNotificationState,
    cnConnectionNotificationType,
    cnConnectionEvents,
    cnServiceId,
    cnVPCEndpointId,
    cnConnectionNotificationId,
    cnConnectionNotificationARN,

    -- * ConversionTask
    ConversionTask (..),
    mkConversionTask,
    ctImportInstance,
    ctState,
    ctStatusMessage,
    ctImportVolume,
    ctConversionTaskId,
    ctExpirationTime,
    ctTags,

    -- * CreateFleetError
    CreateFleetError (..),
    mkCreateFleetError,
    cfeLifecycle,
    cfeLaunchTemplateAndOverrides,
    cfeErrorCode,
    cfeErrorMessage,

    -- * CreateFleetInstance
    CreateFleetInstance (..),
    mkCreateFleetInstance,
    cfiPlatform,
    cfiLifecycle,
    cfiLaunchTemplateAndOverrides,
    cfiInstanceType,
    cfiInstanceIds,

    -- * CreateTransitGatewayVPCAttachmentRequestOptions
    CreateTransitGatewayVPCAttachmentRequestOptions (..),
    mkCreateTransitGatewayVPCAttachmentRequestOptions,
    ctgvaroIPv6Support,
    ctgvaroApplianceModeSupport,
    ctgvaroDNSSupport,

    -- * CreateVolumePermission
    CreateVolumePermission (..),
    mkCreateVolumePermission,
    cvpGroup,
    cvpUserId,

    -- * CreateVolumePermissionModifications
    CreateVolumePermissionModifications (..),
    mkCreateVolumePermissionModifications,
    cvpmRemove,
    cvpmAdd,

    -- * CreditSpecification
    CreditSpecification (..),
    mkCreditSpecification,
    csCPUCredits,

    -- * CreditSpecificationRequest
    CreditSpecificationRequest (..),
    mkCreditSpecificationRequest,
    csrCPUCredits,

    -- * CustomerGateway
    CustomerGateway (..),
    mkCustomerGateway,
    cusCertificateARN,
    cusDeviceName,
    cusTags,
    cusBGPASN,
    cusCustomerGatewayId,
    cusIPAddress,
    cusState,
    cusType,

    -- * DHCPConfiguration
    DHCPConfiguration (..),
    mkDHCPConfiguration,
    dcValues,
    dcKey,

    -- * DHCPOptions
    DHCPOptions (..),
    mkDHCPOptions,
    doDHCPConfigurations,
    doOwnerId,
    doDHCPOptionsId,
    doTags,

    -- * DNSEntry
    DNSEntry (..),
    mkDNSEntry,
    deHostedZoneId,
    deDNSName,

    -- * DNSServersOptionsModifyStructure
    DNSServersOptionsModifyStructure (..),
    mkDNSServersOptionsModifyStructure,
    dsomsEnabled,
    dsomsCustomDNSServers,

    -- * DeleteFleetError
    DeleteFleetError (..),
    mkDeleteFleetError,
    dfeCode,
    dfeMessage,

    -- * DeleteFleetErrorItem
    DeleteFleetErrorItem (..),
    mkDeleteFleetErrorItem,
    dfeiError,
    dfeiFleetId,

    -- * DeleteFleetSuccessItem
    DeleteFleetSuccessItem (..),
    mkDeleteFleetSuccessItem,
    dfsiCurrentFleetState,
    dfsiPreviousFleetState,
    dfsiFleetId,

    -- * DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    mkDeleteLaunchTemplateVersionsResponseErrorItem,
    dltvreiLaunchTemplateName,
    dltvreiLaunchTemplateId,
    dltvreiVersionNumber,
    dltvreiResponseError,

    -- * DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    mkDeleteLaunchTemplateVersionsResponseSuccessItem,
    dltvrsiLaunchTemplateName,
    dltvrsiLaunchTemplateId,
    dltvrsiVersionNumber,

    -- * DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (..),
    mkDeleteQueuedReservedInstancesError,
    dqrieCode,
    dqrieMessage,

    -- * DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    mkDeregisterInstanceTagAttributeRequest,
    ditarIncludeAllTagsOfInstance,
    ditarInstanceTagKeys,

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    mkDescribeFastSnapshotRestoreSuccessItem,
    dfsrsiDisablingTime,
    dfsrsiState,
    dfsrsiOwnerAlias,
    dfsrsiDisabledTime,
    dfsrsiEnabledTime,
    dfsrsiOptimizingTime,
    dfsrsiOwnerId,
    dfsrsiStateTransitionReason,
    dfsrsiAvailabilityZone,
    dfsrsiSnapshotId,
    dfsrsiEnablingTime,

    -- * DescribeFleetError
    DescribeFleetError (..),
    mkDescribeFleetError,
    dfeLifecycle,
    dfeLaunchTemplateAndOverrides,
    dfeErrorCode,
    dfeErrorMessage,

    -- * DescribeFleetsInstances
    DescribeFleetsInstances (..),
    mkDescribeFleetsInstances,
    dfiPlatform,
    dfiLifecycle,
    dfiLaunchTemplateAndOverrides,
    dfiInstanceType,
    dfiInstanceIds,

    -- * DirectoryServiceAuthentication
    DirectoryServiceAuthentication (..),
    mkDirectoryServiceAuthentication,
    dsaDirectoryId,

    -- * DirectoryServiceAuthenticationRequest
    DirectoryServiceAuthenticationRequest (..),
    mkDirectoryServiceAuthenticationRequest,
    dsarDirectoryId,

    -- * DisableFastSnapshotRestoreErrorItem
    DisableFastSnapshotRestoreErrorItem (..),
    mkDisableFastSnapshotRestoreErrorItem,
    dfsreiFastSnapshotRestoreStateErrors,
    dfsreiSnapshotId,

    -- * DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (..),
    mkDisableFastSnapshotRestoreStateError,
    dfsrseCode,
    dfsrseMessage,

    -- * DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    mkDisableFastSnapshotRestoreStateErrorItem,
    dfsrseiError,
    dfsrseiAvailabilityZone,

    -- * DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    mkDisableFastSnapshotRestoreSuccessItem,
    dDisablingTime,
    dState,
    dOwnerAlias,
    dDisabledTime,
    dEnabledTime,
    dOptimizingTime,
    dOwnerId,
    dStateTransitionReason,
    dAvailabilityZone,
    dSnapshotId,
    dEnablingTime,

    -- * DiskImage
    DiskImage (..),
    mkDiskImage,
    diImage,
    diVolume,
    diDescription,

    -- * DiskImageDescription
    DiskImageDescription (..),
    mkDiskImageDescription,
    dSize,
    dChecksum,
    dFormat,
    dImportManifestURL,

    -- * DiskImageDetail
    DiskImageDetail (..),
    mkDiskImageDetail,
    didBytes,
    didFormat,
    didImportManifestURL,

    -- * DiskImageVolumeDescription
    DiskImageVolumeDescription (..),
    mkDiskImageVolumeDescription,
    divdSize,
    divdId,

    -- * DiskInfo
    DiskInfo (..),
    mkDiskInfo,
    diCount,
    diSizeInGB,
    diType,

    -- * EBSBlockDevice
    EBSBlockDevice (..),
    mkEBSBlockDevice,
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdEncrypted,
    ebdKMSKeyId,
    ebdVolumeType,
    ebdSnapshotId,

    -- * EBSInfo
    EBSInfo (..),
    mkEBSInfo,
    eiEBSOptimizedInfo,
    eiEncryptionSupport,
    eiEBSOptimizedSupport,
    eiNvmeSupport,

    -- * EBSInstanceBlockDevice
    EBSInstanceBlockDevice (..),
    mkEBSInstanceBlockDevice,
    eibdStatus,
    eibdDeleteOnTermination,
    eibdVolumeId,
    eibdAttachTime,

    -- * EBSInstanceBlockDeviceSpecification
    EBSInstanceBlockDeviceSpecification (..),
    mkEBSInstanceBlockDeviceSpecification,
    eibdsDeleteOnTermination,
    eibdsVolumeId,

    -- * EBSOptimizedInfo
    EBSOptimizedInfo (..),
    mkEBSOptimizedInfo,
    eoiMaximumIOPS,
    eoiBaselineIOPS,
    eoiMaximumThroughputInMBps,
    eoiMaximumBandwidthInMbps,
    eoiBaselineBandwidthInMbps,
    eoiBaselineThroughputInMBps,

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    mkEgressOnlyInternetGateway,
    eoigEgressOnlyInternetGatewayId,
    eoigAttachments,
    eoigTags,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    mkElasticGpuAssociation,
    egaElasticGpuId,
    egaElasticGpuAssociationId,
    egaElasticGpuAssociationTime,
    egaElasticGpuAssociationState,

    -- * ElasticGpuHealth
    ElasticGpuHealth (..),
    mkElasticGpuHealth,
    eghStatus,

    -- * ElasticGpuSpecification
    ElasticGpuSpecification (..),
    mkElasticGpuSpecification,
    egsType,

    -- * ElasticGpuSpecificationResponse
    ElasticGpuSpecificationResponse (..),
    mkElasticGpuSpecificationResponse,
    eType,

    -- * ElasticGpus
    ElasticGpus (..),
    mkElasticGpus,
    egInstanceId,
    egElasticGpuType,
    egElasticGpuId,
    egElasticGpuState,
    egElasticGpuHealth,
    egAvailabilityZone,
    egTags,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    mkElasticInferenceAccelerator,
    eiaCount,
    eiaType,

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    mkElasticInferenceAcceleratorAssociation,
    eiaaElasticInferenceAcceleratorAssociationState,
    eiaaElasticInferenceAcceleratorAssociationTime,
    eiaaElasticInferenceAcceleratorARN,
    eiaaElasticInferenceAcceleratorAssociationId,

    -- * EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    mkEnableFastSnapshotRestoreErrorItem,
    efsreiFastSnapshotRestoreStateErrors,
    efsreiSnapshotId,

    -- * EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (..),
    mkEnableFastSnapshotRestoreStateError,
    efsrseCode,
    efsrseMessage,

    -- * EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (..),
    mkEnableFastSnapshotRestoreStateErrorItem,
    efsrseiError,
    efsrseiAvailabilityZone,

    -- * EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    mkEnableFastSnapshotRestoreSuccessItem,
    efsrsiDisablingTime,
    efsrsiState,
    efsrsiOwnerAlias,
    efsrsiDisabledTime,
    efsrsiEnabledTime,
    efsrsiOptimizingTime,
    efsrsiOwnerId,
    efsrsiStateTransitionReason,
    efsrsiAvailabilityZone,
    efsrsiSnapshotId,
    efsrsiEnablingTime,

    -- * EnclaveOptions
    EnclaveOptions (..),
    mkEnclaveOptions,
    eoEnabled,

    -- * EnclaveOptionsRequest
    EnclaveOptionsRequest (..),
    mkEnclaveOptionsRequest,
    eorEnabled,

    -- * EventInformation
    EventInformation (..),
    mkEventInformation,
    eiInstanceId,
    eiEventDescription,
    eiEventSubType,

    -- * ExportImageTask
    ExportImageTask (..),
    mkExportImageTask,
    eitStatus,
    eitProgress,
    eitExportImageTaskId,
    eitStatusMessage,
    eitImageId,
    eitDescription,
    eitTags,
    eitS3ExportLocation,

    -- * ExportTask
    ExportTask (..),
    mkExportTask,
    etTags,
    etDescription,
    etExportTaskId,
    etExportToS3Task,
    etInstanceExportDetails,
    etState,
    etStatusMessage,

    -- * ExportTaskS3Location
    ExportTaskS3Location (..),
    mkExportTaskS3Location,
    etslS3Prefix,
    etslS3Bucket,

    -- * ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (..),
    mkExportTaskS3LocationRequest,
    etslrS3Prefix,
    etslrS3Bucket,

    -- * ExportToS3Task
    ExportToS3Task (..),
    mkExportToS3Task,
    etstS3Key,
    etstContainerFormat,
    etstS3Bucket,
    etstDiskImageFormat,

    -- * ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    mkExportToS3TaskSpecification,
    etstsContainerFormat,
    etstsS3Prefix,
    etstsS3Bucket,
    etstsDiskImageFormat,

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    mkFailedQueuedPurchaseDeletion,
    fqpdError,
    fqpdReservedInstancesId,

    -- * FederatedAuthentication
    FederatedAuthentication (..),
    mkFederatedAuthentication,
    faSamlProviderARN,
    faSelfServiceSamlProviderARN,

    -- * FederatedAuthenticationRequest
    FederatedAuthenticationRequest (..),
    mkFederatedAuthenticationRequest,
    farSAMLProviderARN,
    farSelfServiceSAMLProviderARN,

    -- * Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- * FleetData
    FleetData (..),
    mkFleetData,
    fdClientToken,
    fdTargetCapacitySpecification,
    fdSpotOptions,
    fdExcessCapacityTerminationPolicy,
    fdOnDemandOptions,
    fdFleetState,
    fdLaunchTemplateConfigs,
    fdValidUntil,
    fdTerminateInstancesWithExpiration,
    fdInstances,
    fdFulfilledCapacity,
    fdType,
    fdValidFrom,
    fdReplaceUnhealthyInstances,
    fdFulfilledOnDemandCapacity,
    fdFleetId,
    fdErrors,
    fdCreateTime,
    fdTags,
    fdActivityStatus,

    -- * FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (..),
    mkFleetLaunchTemplateConfig,
    fltcOverrides,
    fltcLaunchTemplateSpecification,

    -- * FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (..),
    mkFleetLaunchTemplateConfigRequest,
    fltcrOverrides,
    fltcrLaunchTemplateSpecification,

    -- * FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (..),
    mkFleetLaunchTemplateOverrides,
    fltoPriority,
    fltoWeightedCapacity,
    fltoSubnetId,
    fltoInstanceType,
    fltoAvailabilityZone,
    fltoPlacement,
    fltoMaxPrice,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    mkFleetLaunchTemplateOverridesRequest,
    fltorPriority,
    fltorWeightedCapacity,
    fltorSubnetId,
    fltorInstanceType,
    fltorAvailabilityZone,
    fltorPlacement,
    fltorMaxPrice,

    -- * FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    mkFleetLaunchTemplateSpecification,
    fltsLaunchTemplateName,
    fltsLaunchTemplateId,
    fltsVersion,

    -- * FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    mkFleetLaunchTemplateSpecificationRequest,
    fltsrLaunchTemplateName,
    fltsrLaunchTemplateId,
    fltsrVersion,

    -- * FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (..),
    mkFleetSpotCapacityRebalance,
    fscrReplacementStrategy,

    -- * FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    mkFleetSpotCapacityRebalanceRequest,
    fscrrReplacementStrategy,

    -- * FleetSpotMaintenanceStrategies
    FleetSpotMaintenanceStrategies (..),
    mkFleetSpotMaintenanceStrategies,
    fsmsCapacityRebalance,

    -- * FleetSpotMaintenanceStrategiesRequest
    FleetSpotMaintenanceStrategiesRequest (..),
    mkFleetSpotMaintenanceStrategiesRequest,
    fsmsrCapacityRebalance,

    -- * FlowLog
    FlowLog (..),
    mkFlowLog,
    flCreationTime,
    flLogFormat,
    flMaxAggregationInterval,
    flResourceId,
    flFlowLogStatus,
    flTrafficType,
    flLogDestination,
    flDeliverLogsStatus,
    flDeliverLogsErrorMessage,
    flLogGroupName,
    flDeliverLogsPermissionARN,
    flLogDestinationType,
    flFlowLogId,
    flTags,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    mkFpgaDeviceInfo,
    fdiMemoryInfo,
    fdiManufacturer,
    fdiCount,
    fdiName,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    mkFpgaDeviceMemoryInfo,
    fdmiSizeInMiB,

    -- * FpgaImage
    FpgaImage (..),
    mkFpgaImage,
    fiShellVersion,
    fiPciId,
    fiState,
    fiOwnerAlias,
    fiFpgaImageId,
    fiDataRetentionSupport,
    fiOwnerId,
    fiUpdateTime,
    fiName,
    fiProductCodes,
    fiDescription,
    fiCreateTime,
    fiTags,
    fiPublic,
    fiFpgaImageGlobalId,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    mkFpgaImageAttribute,
    fiaFpgaImageId,
    fiaName,
    fiaProductCodes,
    fiaDescription,
    fiaLoadPermissions,

    -- * FpgaImageState
    FpgaImageState (..),
    mkFpgaImageState,
    fisCode,
    fisMessage,

    -- * FpgaInfo
    FpgaInfo (..),
    mkFpgaInfo,
    fiTotalFpgaMemoryInMiB,
    fiFpgas,

    -- * GpuDeviceInfo
    GpuDeviceInfo (..),
    mkGpuDeviceInfo,
    gdiMemoryInfo,
    gdiManufacturer,
    gdiCount,
    gdiName,

    -- * GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (..),
    mkGpuDeviceMemoryInfo,
    gdmiSizeInMiB,

    -- * GpuInfo
    GpuInfo (..),
    mkGpuInfo,
    giTotalGpuMemoryInMiB,
    giGpus,

    -- * GroupIdentifier
    GroupIdentifier (..),
    mkGroupIdentifier,
    giGroupId,
    giGroupName,

    -- * HibernationOptions
    HibernationOptions (..),
    mkHibernationOptions,
    hoConfigured,

    -- * HibernationOptionsRequest
    HibernationOptionsRequest (..),
    mkHibernationOptionsRequest,
    horConfigured,

    -- * HistoryRecord
    HistoryRecord (..),
    mkHistoryRecord,
    hrEventType,
    hrEventInformation,
    hrTimestamp,

    -- * HistoryRecordEntry
    HistoryRecordEntry (..),
    mkHistoryRecordEntry,
    hreEventType,
    hreEventInformation,
    hreTimestamp,

    -- * Host
    Host (..),
    mkHost,
    hReleaseTime,
    hState,
    hClientToken,
    hAvailabilityZoneId,
    hHostId,
    hAvailableCapacity,
    hHostReservationId,
    hAllowsMultipleInstanceTypes,
    hHostProperties,
    hOwnerId,
    hAvailabilityZone,
    hInstances,
    hAllocationTime,
    hMemberOfServiceLinkedResourceGroup,
    hHostRecovery,
    hAutoPlacement,
    hTags,

    -- * HostInstance
    HostInstance (..),
    mkHostInstance,
    hiInstanceId,
    hiInstanceType,
    hiOwnerId,

    -- * HostOffering
    HostOffering (..),
    mkHostOffering,
    hoInstanceFamily,
    hoCurrencyCode,
    hoHourlyPrice,
    hoUpfrontPrice,
    hoOfferingId,
    hoDuration,
    hoPaymentOption,

    -- * HostProperties
    HostProperties (..),
    mkHostProperties,
    hpInstanceFamily,
    hpInstanceType,
    hpTotalVCPUs,
    hpCores,
    hpSockets,

    -- * HostReservation
    HostReservation (..),
    mkHostReservation,
    hrState,
    hrInstanceFamily,
    hrCurrencyCode,
    hrHostReservationId,
    hrStart,
    hrHourlyPrice,
    hrCount,
    hrUpfrontPrice,
    hrEnd,
    hrHostIdSet,
    hrOfferingId,
    hrDuration,
    hrTags,
    hrPaymentOption,

    -- * IAMInstanceProfile
    IAMInstanceProfile (..),
    mkIAMInstanceProfile,
    iapARN,
    iapId,

    -- * IAMInstanceProfileAssociation
    IAMInstanceProfileAssociation (..),
    mkIAMInstanceProfileAssociation,
    iapaAssociationId,
    iapaInstanceId,
    iapaState,
    iapaIAMInstanceProfile,
    iapaTimestamp,

    -- * IAMInstanceProfileSpecification
    IAMInstanceProfileSpecification (..),
    mkIAMInstanceProfileSpecification,
    iapsARN,
    iapsName,

    -- * ICMPTypeCode
    ICMPTypeCode (..),
    mkICMPTypeCode,
    itcCode,
    itcType,

    -- * IKEVersionsListValue
    IKEVersionsListValue (..),
    mkIKEVersionsListValue,
    ikevlvValue,

    -- * IKEVersionsRequestListValue
    IKEVersionsRequestListValue (..),
    mkIKEVersionsRequestListValue,
    ikevrlvValue,

    -- * IPPermission
    IPPermission (..),
    mkIPPermission,
    ipFromPort,
    ipUserIdGroupPairs,
    ipPrefixListIds,
    ipToPort,
    ipIPv6Ranges,
    ipIPRanges,
    ipIPProtocol,

    -- * IPRange
    IPRange (..),
    mkIPRange,
    iprDescription,
    iprCidrIP,

    -- * IPv6CidrAssociation
    IPv6CidrAssociation (..),
    mkIPv6CidrAssociation,
    icaAssociatedResource,
    icaIPv6Cidr,

    -- * IPv6CidrBlock
    IPv6CidrBlock (..),
    mkIPv6CidrBlock,
    icbIPv6CidrBlock,

    -- * IPv6Pool
    IPv6Pool (..),
    mkIPv6Pool,
    ipPoolCidrBlocks,
    ipPoolId,
    ipDescription,
    ipTags,

    -- * IPv6Range
    IPv6Range (..),
    mkIPv6Range,
    irCidrIPv6,
    irDescription,

    -- * IdFormat
    IdFormat (..),
    mkIdFormat,
    ifUseLongIds,
    ifDeadline,
    ifResource,

    -- * Image
    Image (..),
    mkImage,
    iPlatform,
    iPlatformDetails,
    iEnaSupport,
    iImageOwnerAlias,
    iUsageOperation,
    iRAMDiskId,
    iKernelId,
    iRootDeviceName,
    iSRIOVNetSupport,
    iName,
    iCreationDate,
    iProductCodes,
    iStateReason,
    iDescription,
    iBlockDeviceMappings,
    iTags,
    iImageId,
    iImageLocation,
    iState,
    iOwnerId,
    iPublic,
    iArchitecture,
    iImageType,
    iRootDeviceType,
    iVirtualizationType,
    iHypervisor,

    -- * ImageDiskContainer
    ImageDiskContainer (..),
    mkImageDiskContainer,
    idcFormat,
    idcURL,
    idcDeviceName,
    idcUserBucket,
    idcDescription,
    idcSnapshotId,

    -- * ImportImageLicenseConfigurationRequest
    ImportImageLicenseConfigurationRequest (..),
    mkImportImageLicenseConfigurationRequest,
    iilcrLicenseConfigurationARN,

    -- * ImportImageLicenseConfigurationResponse
    ImportImageLicenseConfigurationResponse (..),
    mkImportImageLicenseConfigurationResponse,
    iilcLicenseConfigurationARN,

    -- * ImportImageTask
    ImportImageTask (..),
    mkImportImageTask,
    iitStatus,
    iitHypervisor,
    iitPlatform,
    iitProgress,
    iitLicenseSpecifications,
    iitLicenseType,
    iitSnapshotDetails,
    iitEncrypted,
    iitKMSKeyId,
    iitStatusMessage,
    iitImageId,
    iitImportTaskId,
    iitArchitecture,
    iitDescription,
    iitTags,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    mkImportInstanceLaunchSpecification,
    iilsAdditionalInfo,
    iilsGroupNames,
    iilsSubnetId,
    iilsInstanceType,
    iilsGroupIds,
    iilsUserData,
    iilsMonitoring,
    iilsPrivateIPAddress,
    iilsInstanceInitiatedShutdownBehavior,
    iilsArchitecture,
    iilsPlacement,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    mkImportInstanceTaskDetails,
    iitdInstanceId,
    iitdPlatform,
    iitdVolumes,
    iitdDescription,

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    mkImportInstanceVolumeDetailItem,
    iivdiStatus,
    iivdiBytesConverted,
    iivdiImage,
    iivdiVolume,
    iivdiAvailabilityZone,
    iivdiStatusMessage,
    iivdiDescription,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    mkImportSnapshotTask,
    istSnapshotTaskDetail,
    istImportTaskId,
    istDescription,
    istTags,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    mkImportVolumeTaskDetails,
    ivtdBytesConverted,
    ivtdImage,
    ivtdVolume,
    ivtdAvailabilityZone,
    ivtdDescription,

    -- * InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    mkInferenceAcceleratorInfo,
    iaiAccelerators,

    -- * InferenceDeviceInfo
    InferenceDeviceInfo (..),
    mkInferenceDeviceInfo,
    idiManufacturer,
    idiCount,
    idiName,

    -- * Instance
    Instance (..),
    mkInstance,
    insPublicDNSName,
    insPlatform,
    insSecurityGroups,
    insClientToken,
    insEnaSupport,
    insSourceDestCheck,
    insElasticGpuAssociations,
    insVPCId,
    insKeyName,
    insNetworkInterfaces,
    insOutpostARN,
    insEnclaveOptions,
    insRAMDiskId,
    insCPUOptions,
    insSubnetId,
    insKernelId,
    insRootDeviceName,
    insCapacityReservationId,
    insCapacityReservationSpecification,
    insSRIOVNetSupport,
    insEBSOptimized,
    insStateTransitionReason,
    insHibernationOptions,
    insInstanceLifecycle,
    insIAMInstanceProfile,
    insPrivateIPAddress,
    insMetadataOptions,
    insProductCodes,
    insSpotInstanceRequestId,
    insLicenses,
    insElasticInferenceAcceleratorAssociations,
    insPrivateDNSName,
    insStateReason,
    insBlockDeviceMappings,
    insPublicIPAddress,
    insTags,
    insInstanceId,
    insImageId,
    insAMILaunchIndex,
    insInstanceType,
    insLaunchTime,
    insPlacement,
    insMonitoring,
    insArchitecture,
    insRootDeviceType,
    insVirtualizationType,
    insHypervisor,
    insState,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    mkInstanceBlockDeviceMapping,
    ibdmEBS,
    ibdmDeviceName,

    -- * InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (..),
    mkInstanceBlockDeviceMappingSpecification,
    ibdmsVirtualName,
    ibdmsNoDevice,
    ibdmsEBS,
    ibdmsDeviceName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    mkInstanceCapacity,
    icAvailableCapacity,
    icInstanceType,
    icTotalCapacity,

    -- * InstanceCount
    InstanceCount (..),
    mkInstanceCount,
    icState,
    icInstanceCount,

    -- * InstanceCreditSpecification
    InstanceCreditSpecification (..),
    mkInstanceCreditSpecification,
    icsInstanceId,
    icsCPUCredits,

    -- * InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (..),
    mkInstanceCreditSpecificationRequest,
    icsrInstanceId,
    icsrCPUCredits,

    -- * InstanceExportDetails
    InstanceExportDetails (..),
    mkInstanceExportDetails,
    iedTargetEnvironment,
    iedInstanceId,

    -- * InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    mkInstanceFamilyCreditSpecification,
    ifcsInstanceFamily,
    ifcsCPUCredits,

    -- * InstanceIPv6Address
    InstanceIPv6Address (..),
    mkInstanceIPv6Address,
    iiaIPv6Address,

    -- * InstanceIPv6AddressRequest
    InstanceIPv6AddressRequest (..),
    mkInstanceIPv6AddressRequest,
    iiarIPv6Address,

    -- * InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    mkInstanceMarketOptionsRequest,
    imorMarketType,
    imorSpotOptions,

    -- * InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    mkInstanceMetadataOptionsRequest,
    imorHTTPEndpoint,
    imorHTTPPutResponseHopLimit,
    imorHTTPTokens,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    mkInstanceMetadataOptionsResponse,
    imoState,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imInstanceId,
    imMonitoring,

    -- * InstanceNetworkInterface
    InstanceNetworkInterface (..),
    mkInstanceNetworkInterface,
    iniGroups,
    iniStatus,
    iniPrivateIPAddresses,
    iniSourceDestCheck,
    iniInterfaceType,
    iniVPCId,
    iniNetworkInterfaceId,
    iniSubnetId,
    iniMACAddress,
    iniAttachment,
    iniOwnerId,
    iniPrivateIPAddress,
    iniPrivateDNSName,
    iniDescription,
    iniAssociation,
    iniIPv6Addresses,

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    mkInstanceNetworkInterfaceAssociation,
    iniaPublicDNSName,
    iniaCarrierIP,
    iniaIPOwnerId,
    iniaPublicIP,

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    mkInstanceNetworkInterfaceAttachment,
    iniaStatus,
    iniaDeleteOnTermination,
    iniaAttachmentId,
    iniaNetworkCardIndex,
    iniaAttachTime,
    iniaDeviceIndex,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    mkInstanceNetworkInterfaceSpecification,
    inisGroups,
    inisPrivateIPAddresses,
    inisDeleteOnTermination,
    inisAssociateCarrierIPAddress,
    inisAssociatePublicIPAddress,
    inisInterfaceType,
    inisNetworkInterfaceId,
    inisSubnetId,
    inisIPv6AddressCount,
    inisNetworkCardIndex,
    inisPrivateIPAddress,
    inisSecondaryPrivateIPAddressCount,
    inisDescription,
    inisDeviceIndex,
    inisIPv6Addresses,

    -- * InstancePrivateIPAddress
    InstancePrivateIPAddress (..),
    mkInstancePrivateIPAddress,
    ipiaPrimary,
    ipiaPrivateIPAddress,
    ipiaPrivateDNSName,
    ipiaAssociation,

    -- * InstanceSpecification
    InstanceSpecification (..),
    mkInstanceSpecification,
    isInstanceId,
    isExcludeBootVolume,

    -- * InstanceState
    InstanceState (..),
    mkInstanceState,
    isName,
    isCode,

    -- * InstanceStateChange
    InstanceStateChange (..),
    mkInstanceStateChange,
    iscInstanceId,
    iscCurrentState,
    iscPreviousState,

    -- * InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    iInstanceId,
    iOutpostARN,
    iSystemStatus,
    iEvents,
    iAvailabilityZone,
    iInstanceStatus,
    iInstanceState,

    -- * InstanceStatusDetails
    InstanceStatusDetails (..),
    mkInstanceStatusDetails,
    isdStatus,
    isdImpairedSince,
    isdName,

    -- * InstanceStatusEvent
    InstanceStatusEvent (..),
    mkInstanceStatusEvent,
    iseNotBefore,
    iseCode,
    iseInstanceEventId,
    iseDescription,
    iseNotBeforeDeadline,
    iseNotAfter,

    -- * InstanceStatusSummary
    InstanceStatusSummary (..),
    mkInstanceStatusSummary,
    issDetails,
    issStatus,

    -- * InstanceStorageInfo
    InstanceStorageInfo (..),
    mkInstanceStorageInfo,
    isiTotalSizeInGB,
    isiNvmeSupport,
    isiDisks,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    mkInstanceTagNotificationAttribute,
    itnaIncludeAllTagsOfInstance,
    itnaInstanceTagKeys,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    mkInstanceTypeInfo,
    itiHypervisor,
    itiCurrentGeneration,
    itiMemoryInfo,
    itiPlacementGroupInfo,
    itiSupportedRootDeviceTypes,
    itiSupportedUsageClasses,
    itiInstanceStorageSupported,
    itiFpgaInfo,
    itiBurstablePerformanceSupported,
    itiInstanceType,
    itiGpuInfo,
    itiSupportedVirtualizationTypes,
    itiEBSInfo,
    itiAutoRecoverySupported,
    itiInferenceAcceleratorInfo,
    itiBareMetal,
    itiNetworkInfo,
    itiProcessorInfo,
    itiFreeTierEligible,
    itiVCPUInfo,
    itiInstanceStorageInfo,
    itiDedicatedHostsSupported,
    itiHibernationSupported,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    mkInstanceTypeOffering,
    itoLocation,
    itoInstanceType,
    itoLocationType,

    -- * InstanceUsage
    InstanceUsage (..),
    mkInstanceUsage,
    iuAccountId,
    iuUsedInstanceCount,

    -- * InternetGateway
    InternetGateway (..),
    mkInternetGateway,
    igAttachments,
    igOwnerId,
    igTags,
    igInternetGatewayId,

    -- * InternetGatewayAttachment
    InternetGatewayAttachment (..),
    mkInternetGatewayAttachment,
    igaState,
    igaVPCId,

    -- * KeyPairInfo
    KeyPairInfo (..),
    mkKeyPairInfo,
    kpiKeyFingerprint,
    kpiKeyName,
    kpiKeyPairId,
    kpiTags,

    -- * LastError
    LastError (..),
    mkLastError,
    leCode,
    leMessage,

    -- * LaunchPermission
    LaunchPermission (..),
    mkLaunchPermission,
    lGroup,
    lUserId,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    mkLaunchPermissionModifications,
    lRemove,
    lAdd,

    -- * LaunchSpecification
    LaunchSpecification (..),
    mkLaunchSpecification,
    lsSecurityGroups,
    lsKeyName,
    lsNetworkInterfaces,
    lsRAMDiskId,
    lsSubnetId,
    lsKernelId,
    lsInstanceType,
    lsEBSOptimized,
    lsUserData,
    lsMonitoring,
    lsIAMInstanceProfile,
    lsImageId,
    lsAddressingType,
    lsBlockDeviceMappings,
    lsPlacement,

    -- * LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltLaunchTemplateName,
    ltLatestVersionNumber,
    ltLaunchTemplateId,
    ltCreatedBy,
    ltDefaultVersionNumber,
    ltCreateTime,
    ltTags,

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    mkLaunchTemplateAndOverridesResponse,
    ltaoOverrides,
    ltaoLaunchTemplateSpecification,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    mkLaunchTemplateBlockDeviceMapping,
    ltbdmVirtualName,
    ltbdmNoDevice,
    ltbdmEBS,
    ltbdmDeviceName,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    mkLaunchTemplateBlockDeviceMappingRequest,
    ltbdmrVirtualName,
    ltbdmrNoDevice,
    ltbdmrEBS,
    ltbdmrDeviceName,

    -- * LaunchTemplateCPUOptions
    LaunchTemplateCPUOptions (..),
    mkLaunchTemplateCPUOptions,
    ltcoCoreCount,
    ltcoThreadsPerCore,

    -- * LaunchTemplateCPUOptionsRequest
    LaunchTemplateCPUOptionsRequest (..),
    mkLaunchTemplateCPUOptionsRequest,
    ltcorCoreCount,
    ltcorThreadsPerCore,

    -- * LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (..),
    mkLaunchTemplateCapacityReservationSpecificationRequest,
    ltcrsrCapacityReservationTarget,
    ltcrsrCapacityReservationPreference,

    -- * LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (..),
    mkLaunchTemplateCapacityReservationSpecificationResponse,
    ltcrsCapacityReservationTarget,
    ltcrsCapacityReservationPreference,

    -- * LaunchTemplateConfig
    LaunchTemplateConfig (..),
    mkLaunchTemplateConfig,
    ltcOverrides,
    ltcLaunchTemplateSpecification,

    -- * LaunchTemplateEBSBlockDevice
    LaunchTemplateEBSBlockDevice (..),
    mkLaunchTemplateEBSBlockDevice,
    ltebdDeleteOnTermination,
    ltebdVolumeSize,
    ltebdIOPS,
    ltebdEncrypted,
    ltebdKMSKeyId,
    ltebdVolumeType,
    ltebdSnapshotId,

    -- * LaunchTemplateEBSBlockDeviceRequest
    LaunchTemplateEBSBlockDeviceRequest (..),
    mkLaunchTemplateEBSBlockDeviceRequest,
    ltebdrDeleteOnTermination,
    ltebdrVolumeSize,
    ltebdrIOPS,
    ltebdrEncrypted,
    ltebdrKMSKeyId,
    ltebdrVolumeType,
    ltebdrSnapshotId,

    -- * LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (..),
    mkLaunchTemplateElasticInferenceAccelerator,
    lteiaCount,
    lteiaType,

    -- * LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (..),
    mkLaunchTemplateElasticInferenceAcceleratorResponse,
    lCount,
    lType,

    -- * LaunchTemplateEnclaveOptions
    LaunchTemplateEnclaveOptions (..),
    mkLaunchTemplateEnclaveOptions,
    lteoEnabled,

    -- * LaunchTemplateEnclaveOptionsRequest
    LaunchTemplateEnclaveOptionsRequest (..),
    mkLaunchTemplateEnclaveOptionsRequest,
    lteorEnabled,

    -- * LaunchTemplateHibernationOptions
    LaunchTemplateHibernationOptions (..),
    mkLaunchTemplateHibernationOptions,
    lthoConfigured,

    -- * LaunchTemplateHibernationOptionsRequest
    LaunchTemplateHibernationOptionsRequest (..),
    mkLaunchTemplateHibernationOptionsRequest,
    lthorConfigured,

    -- * LaunchTemplateIAMInstanceProfileSpecification
    LaunchTemplateIAMInstanceProfileSpecification (..),
    mkLaunchTemplateIAMInstanceProfileSpecification,
    ltiapsARN,
    ltiapsName,

    -- * LaunchTemplateIAMInstanceProfileSpecificationRequest
    LaunchTemplateIAMInstanceProfileSpecificationRequest (..),
    mkLaunchTemplateIAMInstanceProfileSpecificationRequest,
    ltiapsrARN,
    ltiapsrName,

    -- * LaunchTemplateInstanceMarketOptions
    LaunchTemplateInstanceMarketOptions (..),
    mkLaunchTemplateInstanceMarketOptions,
    ltimoMarketType,
    ltimoSpotOptions,

    -- * LaunchTemplateInstanceMarketOptionsRequest
    LaunchTemplateInstanceMarketOptionsRequest (..),
    mkLaunchTemplateInstanceMarketOptionsRequest,
    ltimorMarketType,
    ltimorSpotOptions,

    -- * LaunchTemplateInstanceMetadataOptions
    LaunchTemplateInstanceMetadataOptions (..),
    mkLaunchTemplateInstanceMetadataOptions,
    ltimoState,
    ltimoHTTPEndpoint,
    ltimoHTTPPutResponseHopLimit,
    ltimoHTTPTokens,

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    mkLaunchTemplateInstanceMetadataOptionsRequest,
    ltimorHTTPEndpoint,
    ltimorHTTPPutResponseHopLimit,
    ltimorHTTPTokens,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecification,
    ltinisGroups,
    ltinisPrivateIPAddresses,
    ltinisDeleteOnTermination,
    ltinisAssociateCarrierIPAddress,
    ltinisAssociatePublicIPAddress,
    ltinisInterfaceType,
    ltinisNetworkInterfaceId,
    ltinisSubnetId,
    ltinisIPv6AddressCount,
    ltinisNetworkCardIndex,
    ltinisPrivateIPAddress,
    ltinisSecondaryPrivateIPAddressCount,
    ltinisDescription,
    ltinisDeviceIndex,
    ltinisIPv6Addresses,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    ltinisrGroups,
    ltinisrPrivateIPAddresses,
    ltinisrDeleteOnTermination,
    ltinisrAssociateCarrierIPAddress,
    ltinisrAssociatePublicIPAddress,
    ltinisrInterfaceType,
    ltinisrNetworkInterfaceId,
    ltinisrSubnetId,
    ltinisrIPv6AddressCount,
    ltinisrNetworkCardIndex,
    ltinisrPrivateIPAddress,
    ltinisrSecondaryPrivateIPAddressCount,
    ltinisrDescription,
    ltinisrDeviceIndex,
    ltinisrIPv6Addresses,

    -- * LaunchTemplateLicenseConfiguration
    LaunchTemplateLicenseConfiguration (..),
    mkLaunchTemplateLicenseConfiguration,
    ltlcLicenseConfigurationARN,

    -- * LaunchTemplateLicenseConfigurationRequest
    LaunchTemplateLicenseConfigurationRequest (..),
    mkLaunchTemplateLicenseConfigurationRequest,
    ltlcrLicenseConfigurationARN,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoPriority,
    ltoSpotPrice,
    ltoWeightedCapacity,
    ltoSubnetId,
    ltoInstanceType,
    ltoAvailabilityZone,

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    mkLaunchTemplatePlacement,
    ltpAffinity,
    ltpHostId,
    ltpPartitionNumber,
    ltpSpreadDomain,
    ltpAvailabilityZone,
    ltpTenancy,
    ltpGroupName,
    ltpHostResourceGroupARN,

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    mkLaunchTemplatePlacementRequest,
    ltprAffinity,
    ltprHostId,
    ltprPartitionNumber,
    ltprSpreadDomain,
    ltprAvailabilityZone,
    ltprTenancy,
    ltprGroupName,
    ltprHostResourceGroupARN,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (..),
    mkLaunchTemplateSpotMarketOptions,
    ltsmoBlockDurationMinutes,
    ltsmoInstanceInterruptionBehavior,
    ltsmoValidUntil,
    ltsmoSpotInstanceType,
    ltsmoMaxPrice,

    -- * LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    mkLaunchTemplateSpotMarketOptionsRequest,
    ltsmorBlockDurationMinutes,
    ltsmorInstanceInterruptionBehavior,
    ltsmorValidUntil,
    ltsmorSpotInstanceType,
    ltsmorMaxPrice,

    -- * LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (..),
    mkLaunchTemplateTagSpecification,
    lttsResourceType,
    lttsTags,

    -- * LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (..),
    mkLaunchTemplateTagSpecificationRequest,
    lttsrResourceType,
    lttsrTags,

    -- * LaunchTemplateVersion
    LaunchTemplateVersion (..),
    mkLaunchTemplateVersion,
    ltvLaunchTemplateName,
    ltvLaunchTemplateId,
    ltvCreatedBy,
    ltvDefaultVersion,
    ltvVersionNumber,
    ltvVersionDescription,
    ltvLaunchTemplateData,
    ltvCreateTime,

    -- * LaunchTemplatesMonitoring
    LaunchTemplatesMonitoring (..),
    mkLaunchTemplatesMonitoring,
    ltmEnabled,

    -- * LaunchTemplatesMonitoringRequest
    LaunchTemplatesMonitoringRequest (..),
    mkLaunchTemplatesMonitoringRequest,
    ltmrEnabled,

    -- * LicenseConfiguration
    LicenseConfiguration (..),
    mkLicenseConfiguration,
    lcLicenseConfigurationARN,

    -- * LicenseConfigurationRequest
    LicenseConfigurationRequest (..),
    mkLicenseConfigurationRequest,
    lcrLicenseConfigurationARN,

    -- * LoadBalancersConfig
    LoadBalancersConfig (..),
    mkLoadBalancersConfig,
    lbcClassicLoadBalancersConfig,
    lbcTargetGroupsConfig,

    -- * LoadPermission
    LoadPermission (..),
    mkLoadPermission,
    lpGroup,
    lpUserId,

    -- * LoadPermissionModifications
    LoadPermissionModifications (..),
    mkLoadPermissionModifications,
    lpmRemove,
    lpmAdd,

    -- * LoadPermissionRequest
    LoadPermissionRequest (..),
    mkLoadPermissionRequest,
    lprGroup,
    lprUserId,

    -- * LocalGateway
    LocalGateway (..),
    mkLocalGateway,
    lgState,
    lgLocalGatewayId,
    lgOutpostARN,
    lgOwnerId,
    lgTags,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    mkLocalGatewayRoute,
    lgrState,
    lgrLocalGatewayRouteTableARN,
    lgrOwnerId,
    lgrLocalGatewayRouteTableId,
    lgrType,
    lgrLocalGatewayVirtualInterfaceGroupId,
    lgrDestinationCidrBlock,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    mkLocalGatewayRouteTable,
    lgrtState,
    lgrtLocalGatewayRouteTableARN,
    lgrtLocalGatewayId,
    lgrtOutpostARN,
    lgrtOwnerId,
    lgrtLocalGatewayRouteTableId,
    lgrtTags,

    -- * LocalGatewayRouteTableVPCAssociation
    LocalGatewayRouteTableVPCAssociation (..),
    mkLocalGatewayRouteTableVPCAssociation,
    lgrtvaState,
    lgrtvaLocalGatewayRouteTableARN,
    lgrtvaVPCId,
    lgrtvaLocalGatewayId,
    lgrtvaLocalGatewayRouteTableVPCAssociationId,
    lgrtvaOwnerId,
    lgrtvaLocalGatewayRouteTableId,
    lgrtvaTags,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    lgrtvigaState,
    lgrtvigaLocalGatewayRouteTableARN,
    lgrtvigaLocalGatewayId,
    lgrtvigaOwnerId,
    lgrtvigaLocalGatewayRouteTableId,
    lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId,
    lgrtvigaLocalGatewayVirtualInterfaceGroupId,
    lgrtvigaTags,

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    mkLocalGatewayVirtualInterface,
    lgviLocalGatewayVirtualInterfaceId,
    lgviLocalBGPASN,
    lgviVLAN,
    lgviLocalGatewayId,
    lgviLocalAddress,
    lgviPeerBGPASN,
    lgviOwnerId,
    lgviPeerAddress,
    lgviTags,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    mkLocalGatewayVirtualInterfaceGroup,
    lgvigLocalGatewayId,
    lgvigOwnerId,
    lgvigLocalGatewayVirtualInterfaceIds,
    lgvigLocalGatewayVirtualInterfaceGroupId,
    lgvigTags,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    mkManagedPrefixList,
    mplStateMessage,
    mplState,
    mplPrefixListARN,
    mplAddressFamily,
    mplOwnerId,
    mplPrefixListId,
    mplVersion,
    mplPrefixListName,
    mplMaxEntries,
    mplTags,

    -- * MemoryInfo
    MemoryInfo (..),
    mkMemoryInfo,
    miSizeInMiB,

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    mkModifyTransitGatewayOptions,
    mtgoVPNEcmpSupport,
    mtgoAutoAcceptSharedAttachments,
    mtgoPropagationDefaultRouteTableId,
    mtgoDefaultRouteTableAssociation,
    mtgoAssociationDefaultRouteTableId,
    mtgoDefaultRouteTablePropagation,
    mtgoDNSSupport,

    -- * ModifyTransitGatewayVPCAttachmentRequestOptions
    ModifyTransitGatewayVPCAttachmentRequestOptions (..),
    mkModifyTransitGatewayVPCAttachmentRequestOptions,
    mtgvaroIPv6Support,
    mtgvaroApplianceModeSupport,
    mtgvaroDNSSupport,

    -- * ModifyVPNTunnelOptionsSpecification
    ModifyVPNTunnelOptionsSpecification (..),
    mkModifyVPNTunnelOptionsSpecification,
    mvtosReplayWindowSize,
    mvtosDPDTimeoutAction,
    mvtosRekeyFuzzPercentage,
    mvtosPhase1LifetimeSeconds,
    mvtosIKEVersions,
    mvtosPhase2IntegrityAlgorithms,
    mvtosPhase2LifetimeSeconds,
    mvtosPhase1EncryptionAlgorithms,
    mvtosPhase1DHGroupNumbers,
    mvtosPhase1IntegrityAlgorithms,
    mvtosRekeyMarginTimeSeconds,
    mvtosDPDTimeoutSeconds,
    mvtosTunnelInsideCidr,
    mvtosStartupAction,
    mvtosPhase2EncryptionAlgorithms,
    mvtosPhase2DHGroupNumbers,
    mvtosPreSharedKey,
    mvtosTunnelInsideIPv6Cidr,

    -- * Monitoring
    Monitoring (..),
    mkMonitoring,
    mState,

    -- * MovingAddressStatus
    MovingAddressStatus (..),
    mkMovingAddressStatus,
    masMoveStatus,
    masPublicIP,

    -- * NatGateway
    NatGateway (..),
    mkNatGateway,
    ngState,
    ngFailureCode,
    ngVPCId,
    ngFailureMessage,
    ngNatGatewayId,
    ngSubnetId,
    ngDeleteTime,
    ngProvisionedBandwidth,
    ngNatGatewayAddresses,
    ngCreateTime,
    ngTags,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    mkNatGatewayAddress,
    ngaPrivateIP,
    ngaAllocationId,
    ngaNetworkInterfaceId,
    ngaPublicIP,

    -- * NetworkACL
    NetworkACL (..),
    mkNetworkACL,
    naEntries,
    naNetworkACLId,
    naVPCId,
    naOwnerId,
    naAssociations,
    naTags,
    naIsDefault,

    -- * NetworkACLAssociation
    NetworkACLAssociation (..),
    mkNetworkACLAssociation,
    naaNetworkACLId,
    naaSubnetId,
    naaNetworkACLAssociationId,

    -- * NetworkACLEntry
    NetworkACLEntry (..),
    mkNetworkACLEntry,
    naeIPv6CidrBlock,
    naeICMPTypeCode,
    naeRuleNumber,
    naeRuleAction,
    naeProtocol,
    naePortRange,
    naeCidrBlock,
    naeEgress,

    -- * NetworkCardInfo
    NetworkCardInfo (..),
    mkNetworkCardInfo,
    nciMaximumNetworkInterfaces,
    nciNetworkPerformance,
    nciNetworkCardIndex,

    -- * NetworkInfo
    NetworkInfo (..),
    mkNetworkInfo,
    niEfaSupported,
    niIPv6Supported,
    niEnaSupport,
    niMaximumNetworkInterfaces,
    niIPv6AddressesPerInterface,
    niNetworkPerformance,
    niMaximumNetworkCards,
    niNetworkCards,
    niDefaultNetworkCardIndex,
    niIPv4AddressesPerInterface,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niGroups,
    niStatus,
    niPrivateIPAddresses,
    niSourceDestCheck,
    niInterfaceType,
    niVPCId,
    niTagSet,
    niRequesterManaged,
    niOutpostARN,
    niNetworkInterfaceId,
    niSubnetId,
    niMACAddress,
    niAttachment,
    niOwnerId,
    niAvailabilityZone,
    niPrivateIPAddress,
    niPrivateDNSName,
    niRequesterId,
    niDescription,
    niAssociation,
    niIPv6Addresses,

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    mkNetworkInterfaceAssociation,
    niaAssociationId,
    niaPublicDNSName,
    niaAllocationId,
    niaCarrierIP,
    niaIPOwnerId,
    niaCustomerOwnedIP,
    niaPublicIP,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    mkNetworkInterfaceAttachment,
    niaInstanceId,
    niaStatus,
    niaDeleteOnTermination,
    niaAttachmentId,
    niaNetworkCardIndex,
    niaInstanceOwnerId,
    niaAttachTime,
    niaDeviceIndex,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    mkNetworkInterfaceAttachmentChanges,
    niacDeleteOnTermination,
    niacAttachmentId,

    -- * NetworkInterfaceIPv6Address
    NetworkInterfaceIPv6Address (..),
    mkNetworkInterfaceIPv6Address,
    niiaIPv6Address,

    -- * NetworkInterfacePermission
    NetworkInterfacePermission (..),
    mkNetworkInterfacePermission,
    nipPermissionState,
    nipNetworkInterfacePermissionId,
    nipNetworkInterfaceId,
    nipAWSAccountId,
    nipAWSService,
    nipPermission,

    -- * NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    mkNetworkInterfacePermissionState,
    nipsState,
    nipsStatusMessage,

    -- * NetworkInterfacePrivateIPAddress
    NetworkInterfacePrivateIPAddress (..),
    mkNetworkInterfacePrivateIPAddress,
    nipiaPrimary,
    nipiaPrivateIPAddress,
    nipiaPrivateDNSName,
    nipiaAssociation,

    -- * NewDHCPConfiguration
    NewDHCPConfiguration (..),
    mkNewDHCPConfiguration,
    ndcValues,
    ndcKey,

    -- * OnDemandOptions
    OnDemandOptions (..),
    mkOnDemandOptions,
    odoCapacityReservationOptions,
    odoSingleAvailabilityZone,
    odoMaxTotalPrice,
    odoMinTargetCapacity,
    odoSingleInstanceType,
    odoAllocationStrategy,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    mkOnDemandOptionsRequest,
    odorCapacityReservationOptions,
    odorSingleAvailabilityZone,
    odorMaxTotalPrice,
    odorMinTargetCapacity,
    odorSingleInstanceType,
    odorAllocationStrategy,

    -- * PciId
    PciId (..),
    mkPciId,
    piSubsystemId,
    piDeviceId,
    piSubsystemVendorId,
    piVendorId,

    -- * PeeringAttachmentStatus
    PeeringAttachmentStatus (..),
    mkPeeringAttachmentStatus,
    pasCode,
    pasMessage,

    -- * PeeringConnectionOptions
    PeeringConnectionOptions (..),
    mkPeeringConnectionOptions,
    pcoAllowEgressFromLocalVPCToRemoteClassicLink,
    pcoAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcoAllowDNSResolutionFromRemoteVPC,

    -- * PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    mkPeeringConnectionOptionsRequest,
    pcorAllowEgressFromLocalVPCToRemoteClassicLink,
    pcorAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcorAllowDNSResolutionFromRemoteVPC,

    -- * PeeringTgwInfo
    PeeringTgwInfo (..),
    mkPeeringTgwInfo,
    ptiOwnerId,
    ptiTransitGatewayId,
    ptiRegion,

    -- * Phase1DHGroupNumbersListValue
    Phase1DHGroupNumbersListValue (..),
    mkPhase1DHGroupNumbersListValue,
    pdhgnlvValue,

    -- * Phase1DHGroupNumbersRequestListValue
    Phase1DHGroupNumbersRequestListValue (..),
    mkPhase1DHGroupNumbersRequestListValue,
    pdhgnrlvValue,

    -- * Phase1EncryptionAlgorithmsListValue
    Phase1EncryptionAlgorithmsListValue (..),
    mkPhase1EncryptionAlgorithmsListValue,
    pealveValue,

    -- * Phase1EncryptionAlgorithmsRequestListValue
    Phase1EncryptionAlgorithmsRequestListValue (..),
    mkPhase1EncryptionAlgorithmsRequestListValue,
    pearlvValue,

    -- * Phase1IntegrityAlgorithmsListValue
    Phase1IntegrityAlgorithmsListValue (..),
    mkPhase1IntegrityAlgorithmsListValue,
    pialvValue,

    -- * Phase1IntegrityAlgorithmsRequestListValue
    Phase1IntegrityAlgorithmsRequestListValue (..),
    mkPhase1IntegrityAlgorithmsRequestListValue,
    piarlviValue,

    -- * Phase2DHGroupNumbersListValue
    Phase2DHGroupNumbersListValue (..),
    mkPhase2DHGroupNumbersListValue,
    pValue,

    -- * Phase2DHGroupNumbersRequestListValue
    Phase2DHGroupNumbersRequestListValue (..),
    mkPhase2DHGroupNumbersRequestListValue,
    pdhgnrlvdValue,

    -- * Phase2EncryptionAlgorithmsListValue
    Phase2EncryptionAlgorithmsListValue (..),
    mkPhase2EncryptionAlgorithmsListValue,
    pealvValue,

    -- * Phase2EncryptionAlgorithmsRequestListValue
    Phase2EncryptionAlgorithmsRequestListValue (..),
    mkPhase2EncryptionAlgorithmsRequestListValue,
    pearlveValue,

    -- * Phase2IntegrityAlgorithmsListValue
    Phase2IntegrityAlgorithmsListValue (..),
    mkPhase2IntegrityAlgorithmsListValue,
    phaValue,

    -- * Phase2IntegrityAlgorithmsRequestListValue
    Phase2IntegrityAlgorithmsRequestListValue (..),
    mkPhase2IntegrityAlgorithmsRequestListValue,
    piarlvValue,

    -- * Placement
    Placement (..),
    mkPlacement,
    plaAffinity,
    plaHostId,
    plaPartitionNumber,
    plaSpreadDomain,
    plaAvailabilityZone,
    plaTenancy,
    plaGroupName,
    plaHostResourceGroupARN,

    -- * PlacementGroup
    PlacementGroup (..),
    mkPlacementGroup,
    pgState,
    pgStrategy,
    pgGroupId,
    pgGroupName,
    pgPartitionCount,
    pgTags,

    -- * PlacementGroupInfo
    PlacementGroupInfo (..),
    mkPlacementGroupInfo,
    pgiSupportedStrategies,

    -- * PlacementResponse
    PlacementResponse (..),
    mkPlacementResponse,
    pGroupName,

    -- * PoolCidrBlock
    PoolCidrBlock (..),
    mkPoolCidrBlock,
    pcbCidr,

    -- * PortRange
    PortRange (..),
    mkPortRange,
    prTo,
    prFrom,

    -- * PrefixList
    PrefixList (..),
    mkPrefixList,
    plCidrs,
    plPrefixListId,
    plPrefixListName,

    -- * PrefixListAssociation
    PrefixListAssociation (..),
    mkPrefixListAssociation,
    plaResourceId,
    plaResourceOwner,

    -- * PrefixListEntry
    PrefixListEntry (..),
    mkPrefixListEntry,
    pleCidr,
    pleDescription,

    -- * PrefixListId
    PrefixListId (..),
    mkPrefixListId,
    pliPrefixListId,
    pliDescription,

    -- * PriceSchedule
    PriceSchedule (..),
    mkPriceSchedule,
    psCurrencyCode,
    psTerm,
    psActive,
    psPrice,

    -- * PriceScheduleSpecification
    PriceScheduleSpecification (..),
    mkPriceScheduleSpecification,
    pssCurrencyCode,
    pssTerm,
    pssPrice,

    -- * PricingDetail
    PricingDetail (..),
    mkPricingDetail,
    pdCount,
    pdPrice,

    -- * PrincipalIdFormat
    PrincipalIdFormat (..),
    mkPrincipalIdFormat,
    pifARN,
    pifStatuses,

    -- * PrivateDNSDetails
    PrivateDNSDetails (..),
    mkPrivateDNSDetails,
    pddPrivateDNSName,

    -- * PrivateDNSNameConfiguration
    PrivateDNSNameConfiguration (..),
    mkPrivateDNSNameConfiguration,
    pdncState,
    pdncValue,
    pdncName,
    pdncType,

    -- * PrivateIPAddressSpecification
    PrivateIPAddressSpecification (..),
    mkPrivateIPAddressSpecification,
    piasPrimary,
    piasPrivateIPAddress,

    -- * ProcessorInfo
    ProcessorInfo (..),
    mkProcessorInfo,
    piSupportedArchitectures,
    piSustainedClockSpeedInGhz,

    -- * ProductCode
    ProductCode (..),
    mkProductCode,
    pcProductCodeType,
    pcProductCodeId,

    -- * PropagatingVGW
    PropagatingVGW (..),
    mkPropagatingVGW,
    pvGatewayId,

    -- * ProvisionedBandwidth
    ProvisionedBandwidth (..),
    mkProvisionedBandwidth,
    pbStatus,
    pbRequested,
    pbProvisioned,
    pbRequestTime,
    pbProvisionTime,

    -- * PublicIPv4Pool
    PublicIPv4Pool (..),
    mkPublicIPv4Pool,
    pipTotalAddressCount,
    pipNetworkBorderGroup,
    pipTotalAvailableAddressCount,
    pipPoolAddressRanges,
    pipPoolId,
    pipDescription,
    pipTags,

    -- * PublicIPv4PoolRange
    PublicIPv4PoolRange (..),
    mkPublicIPv4PoolRange,
    piprAvailableAddressCount,
    piprLastAddress,
    piprFirstAddress,
    piprAddressCount,

    -- * Purchase
    Purchase (..),
    mkPurchase,
    pInstanceFamily,
    pCurrencyCode,
    pHostReservationId,
    pHourlyPrice,
    pUpfrontPrice,
    pHostIdSet,
    pDuration,
    pPaymentOption,

    -- * PurchaseRequest
    PurchaseRequest (..),
    mkPurchaseRequest,
    prInstanceCount,
    prPurchaseToken,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcAmount,
    rcFrequency,

    -- * RegionInfo
    RegionInfo (..),
    mkRegionInfo,
    riRegionName,
    riOptInStatus,
    riEndpoint,

    -- * RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    mkRegisterInstanceTagAttributeRequest,
    ritarIncludeAllTagsOfInstance,
    ritarInstanceTagKeys,

    -- * RemovePrefixListEntry
    RemovePrefixListEntry (..),
    mkRemovePrefixListEntry,
    rpleCidr,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    mkRequestLaunchTemplateData,
    rltdSecurityGroupIds,
    rltdSecurityGroups,
    rltdElasticInferenceAccelerators,
    rltdInstanceMarketOptions,
    rltdLicenseSpecifications,
    rltdDisableAPITermination,
    rltdKeyName,
    rltdNetworkInterfaces,
    rltdEnclaveOptions,
    rltdCPUOptions,
    rltdRamDiskId,
    rltdKernelId,
    rltdElasticGpuSpecifications,
    rltdInstanceType,
    rltdCapacityReservationSpecification,
    rltdEBSOptimized,
    rltdUserData,
    rltdMonitoring,
    rltdTagSpecifications,
    rltdHibernationOptions,
    rltdIAMInstanceProfile,
    rltdImageId,
    rltdInstanceInitiatedShutdownBehavior,
    rltdMetadataOptions,
    rltdCreditSpecification,
    rltdBlockDeviceMappings,
    rltdPlacement,

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    mkRequestSpotLaunchSpecification,
    rslsSecurityGroupIds,
    rslsSecurityGroups,
    rslsKeyName,
    rslsNetworkInterfaces,
    rslsRAMDiskId,
    rslsSubnetId,
    rslsKernelId,
    rslsInstanceType,
    rslsEBSOptimized,
    rslsUserData,
    rslsMonitoring,
    rslsIAMInstanceProfile,
    rslsImageId,
    rslsAddressingType,
    rslsBlockDeviceMappings,
    rslsPlacement,

    -- * Reservation
    Reservation (..),
    mkReservation,
    rGroups,
    rInstances,
    rRequesterId,
    rReservationId,
    rOwnerId,

    -- * ReservationValue
    ReservationValue (..),
    mkReservationValue,
    rvHourlyPrice,
    rvRemainingTotalValue,
    rvRemainingUpfrontValue,

    -- * ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (..),
    mkReservedInstanceLimitPrice,
    rilpAmount,
    rilpCurrencyCode,

    -- * ReservedInstanceReservationValue
    ReservedInstanceReservationValue (..),
    mkReservedInstanceReservationValue,
    rirvReservationValue,
    rirvReservedInstanceId,

    -- * ReservedInstances
    ReservedInstances (..),
    mkReservedInstances,
    riState,
    riCurrencyCode,
    riInstanceCount,
    riProductDescription,
    riStart,
    riInstanceType,
    riEnd,
    riAvailabilityZone,
    riScope,
    riRecurringCharges,
    riOfferingType,
    riUsagePrice,
    riFixedPrice,
    riReservedInstancesId,
    riInstanceTenancy,
    riOfferingClass,
    riDuration,
    riTags,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    mkReservedInstancesConfiguration,
    ricPlatform,
    ricInstanceCount,
    ricInstanceType,
    ricAvailabilityZone,
    ricScope,

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    mkReservedInstancesId,
    riiReservedInstancesId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    mkReservedInstancesListing,
    rilStatus,
    rilClientToken,
    rilUpdateDate,
    rilCreateDate,
    rilPriceSchedules,
    rilStatusMessage,
    rilReservedInstancesId,
    rilTags,
    rilInstanceCounts,
    rilReservedInstancesListingId,

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    mkReservedInstancesModification,
    rimModificationResults,
    rimStatus,
    rimClientToken,
    rimUpdateDate,
    rimCreateDate,
    rimEffectiveDate,
    rimStatusMessage,
    rimReservedInstancesModificationId,
    rimReservedInstancesIds,

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    mkReservedInstancesModificationResult,
    rimrReservedInstancesId,
    rimrTargetConfiguration,

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    mkReservedInstancesOffering,
    rioMarketplace,
    rioCurrencyCode,
    rioProductDescription,
    rioInstanceType,
    rioAvailabilityZone,
    rioPricingDetails,
    rioScope,
    rioRecurringCharges,
    rioOfferingType,
    rioUsagePrice,
    rioFixedPrice,
    rioInstanceTenancy,
    rioReservedInstancesOfferingId,
    rioOfferingClass,
    rioDuration,

    -- * ResponseError
    ResponseError (..),
    mkResponseError,
    reCode,
    reMessage,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    mkResponseLaunchTemplateData,
    rSecurityGroupIds,
    rSecurityGroups,
    rElasticInferenceAccelerators,
    rInstanceMarketOptions,
    rLicenseSpecifications,
    rDisableAPITermination,
    rKeyName,
    rNetworkInterfaces,
    rEnclaveOptions,
    rCPUOptions,
    rRamDiskId,
    rKernelId,
    rElasticGpuSpecifications,
    rInstanceType,
    rCapacityReservationSpecification,
    rEBSOptimized,
    rUserData,
    rMonitoring,
    rTagSpecifications,
    rHibernationOptions,
    rIAMInstanceProfile,
    rImageId,
    rInstanceInitiatedShutdownBehavior,
    rMetadataOptions,
    rCreditSpecification,
    rBlockDeviceMappings,
    rPlacement,

    -- * Route
    Route (..),
    mkRoute,
    rVPCPeeringConnectionId,
    rInstanceId,
    rOrigin,
    rState,
    rEgressOnlyInternetGatewayId,
    rDestinationIPv6CidrBlock,
    rLocalGatewayId,
    rNatGatewayId,
    rNetworkInterfaceId,
    rTransitGatewayId,
    rGatewayId,
    rInstanceOwnerId,
    rDestinationPrefixListId,
    rCarrierGatewayId,
    rDestinationCidrBlock,

    -- * RouteTable
    RouteTable (..),
    mkRouteTable,
    rtRouteTableId,
    rtRoutes,
    rtVPCId,
    rtPropagatingVGWs,
    rtOwnerId,
    rtAssociations,
    rtTags,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    mkRouteTableAssociation,
    rtaRouteTableId,
    rtaRouteTableAssociationId,
    rtaMain,
    rtaSubnetId,
    rtaGatewayId,
    rtaAssociationState,

    -- * RouteTableAssociationState
    RouteTableAssociationState (..),
    mkRouteTableAssociationState,
    rtasState,
    rtasStatusMessage,

    -- * RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (..),
    mkRunInstancesMonitoringEnabled,
    rimeEnabled,

    -- * S3Storage
    S3Storage (..),
    mkS3Storage,
    ssPrefix,
    ssUploadPolicy,
    ssBucket,
    ssUploadPolicySignature,
    ssAWSAccessKeyId,

    -- * ScheduledInstance
    ScheduledInstance (..),
    mkScheduledInstance,
    siPreviousSlotEndTime,
    siPlatform,
    siTermStartDate,
    siInstanceCount,
    siScheduledInstanceId,
    siHourlyPrice,
    siCreateDate,
    siSlotDurationInHours,
    siTotalScheduledInstanceHours,
    siInstanceType,
    siRecurrence,
    siAvailabilityZone,
    siTermEndDate,
    siNextSlotStartTime,
    siNetworkPlatform,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    mkScheduledInstanceAvailability,
    siaMaxTermDurationInDays,
    siaPlatform,
    siaPurchaseToken,
    siaHourlyPrice,
    siaAvailableInstanceCount,
    siaSlotDurationInHours,
    siaTotalScheduledInstanceHours,
    siaInstanceType,
    siaRecurrence,
    siaAvailabilityZone,
    siaMinTermDurationInDays,
    siaFirstSlotStartTime,
    siaNetworkPlatform,

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    mkScheduledInstanceRecurrence,
    sirFrequency,
    sirOccurrenceRelativeToEnd,
    sirOccurrenceUnit,
    sirInterval,
    sirOccurrenceDaySet,

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    mkScheduledInstanceRecurrenceRequest,
    sirrFrequency,
    sirrOccurrenceRelativeToEnd,
    sirrOccurrenceDays,
    sirrOccurrenceUnit,
    sirrInterval,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    mkScheduledInstancesBlockDeviceMapping,
    sibdmVirtualName,
    sibdmNoDevice,
    sibdmEBS,
    sibdmDeviceName,

    -- * ScheduledInstancesEBS
    ScheduledInstancesEBS (..),
    mkScheduledInstancesEBS,
    sieDeleteOnTermination,
    sieVolumeSize,
    sieIOPS,
    sieEncrypted,
    sieVolumeType,
    sieSnapshotId,

    -- * ScheduledInstancesIAMInstanceProfile
    ScheduledInstancesIAMInstanceProfile (..),
    mkScheduledInstancesIAMInstanceProfile,
    siiapARN,
    siiapName,

    -- * ScheduledInstancesIPv6Address
    ScheduledInstancesIPv6Address (..),
    mkScheduledInstancesIPv6Address,
    siiaIPv6Address,

    -- * ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (..),
    mkScheduledInstancesLaunchSpecification,
    silsSecurityGroupIds,
    silsKeyName,
    silsNetworkInterfaces,
    silsRAMDiskId,
    silsSubnetId,
    silsKernelId,
    silsInstanceType,
    silsEBSOptimized,
    silsUserData,
    silsMonitoring,
    silsIAMInstanceProfile,
    silsBlockDeviceMappings,
    silsPlacement,
    silsImageId,

    -- * ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    mkScheduledInstancesMonitoring,
    simEnabled,

    -- * ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    mkScheduledInstancesNetworkInterface,
    siniGroups,
    siniDeleteOnTermination,
    siniAssociatePublicIPAddress,
    siniPrivateIPAddressConfigs,
    siniNetworkInterfaceId,
    siniSubnetId,
    siniIPv6AddressCount,
    siniPrivateIPAddress,
    siniSecondaryPrivateIPAddressCount,
    siniDescription,
    siniDeviceIndex,
    siniIPv6Addresses,

    -- * ScheduledInstancesPlacement
    ScheduledInstancesPlacement (..),
    mkScheduledInstancesPlacement,
    sipAvailabilityZone,
    sipGroupName,

    -- * ScheduledInstancesPrivateIPAddressConfig
    ScheduledInstancesPrivateIPAddressConfig (..),
    mkScheduledInstancesPrivateIPAddressConfig,
    sipiacPrimary,
    sipiacPrivateIPAddress,

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgVPCId,
    sgIPPermissions,
    sgIPPermissionsEgress,
    sgTags,
    sgOwnerId,
    sgGroupId,
    sgGroupName,
    sgDescription,

    -- * SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    mkSecurityGroupIdentifier,
    sgiGroupId,
    sgiGroupName,

    -- * SecurityGroupReference
    SecurityGroupReference (..),
    mkSecurityGroupReference,
    sgrVPCPeeringConnectionId,
    sgrReferencingVPCId,
    sgrGroupId,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    mkServiceConfiguration,
    scNetworkLoadBalancerARNs,
    scBaseEndpointDNSNames,
    scAvailabilityZones,
    scGatewayLoadBalancerARNs,
    scManagesVPCEndpoints,
    scServiceName,
    scServiceState,
    scServiceType,
    scAcceptanceRequired,
    scServiceId,
    scPrivateDNSName,
    scPrivateDNSNameConfiguration,
    scTags,

    -- * ServiceDetail
    ServiceDetail (..),
    mkServiceDetail,
    sdPrivateDNSNameVerificationState,
    sdVPCEndpointPolicySupported,
    sdBaseEndpointDNSNames,
    sdOwner,
    sdAvailabilityZones,
    sdManagesVPCEndpoints,
    sdServiceName,
    sdServiceType,
    sdAcceptanceRequired,
    sdPrivateDNSNames,
    sdServiceId,
    sdPrivateDNSName,
    sdTags,

    -- * ServiceTypeDetail
    ServiceTypeDetail (..),
    mkServiceTypeDetail,
    stdServiceType,

    -- * SlotDateTimeRangeRequest
    SlotDateTimeRangeRequest (..),
    mkSlotDateTimeRangeRequest,
    sdtrrEarliestTime,
    sdtrrLatestTime,

    -- * SlotStartTimeRangeRequest
    SlotStartTimeRangeRequest (..),
    mkSlotStartTimeRangeRequest,
    sstrrLatestTime,
    sstrrEarliestTime,

    -- * Snapshot
    Snapshot (..),
    mkSnapshot,
    sStateMessage,
    sOwnerAlias,
    sDataEncryptionKeyId,
    sKMSKeyId,
    sTags,
    sSnapshotId,
    sOwnerId,
    sVolumeId,
    sVolumeSize,
    sDescription,
    sStartTime,
    sProgress,
    sState,
    sEncrypted,

    -- * SnapshotDetail
    SnapshotDetail (..),
    mkSnapshotDetail,
    sdStatus,
    sdProgress,
    sdFormat,
    sdURL,
    sdDeviceName,
    sdStatusMessage,
    sdUserBucket,
    sdDiskImageSize,
    sdDescription,
    sdSnapshotId,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    mkSnapshotDiskContainer,
    sdcFormat,
    sdcURL,
    sdcUserBucket,
    sdcDescription,

    -- * SnapshotInfo
    SnapshotInfo (..),
    mkSnapshotInfo,
    siState,
    siProgress,
    siStartTime,
    siVolumeSize,
    siEncrypted,
    siOwnerId,
    siVolumeId,
    siDescription,
    siTags,
    siSnapshotId,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    mkSnapshotTaskDetail,
    stdStatus,
    stdProgress,
    stdFormat,
    stdURL,
    stdEncrypted,
    stdKMSKeyId,
    stdStatusMessage,
    stdUserBucket,
    stdDiskImageSize,
    stdDescription,
    stdSnapshotId,

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    mkSpotCapacityRebalance,
    scrReplacementStrategy,

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    mkSpotDatafeedSubscription,
    sdsState,
    sdsPrefix,
    sdsBucket,
    sdsOwnerId,
    sdsFault,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    mkSpotFleetLaunchSpecification,
    sflsSecurityGroups,
    sflsSpotPrice,
    sflsWeightedCapacity,
    sflsKeyName,
    sflsNetworkInterfaces,
    sflsRAMDiskId,
    sflsSubnetId,
    sflsKernelId,
    sflsInstanceType,
    sflsEBSOptimized,
    sflsUserData,
    sflsMonitoring,
    sflsTagSpecifications,
    sflsIAMInstanceProfile,
    sflsImageId,
    sflsAddressingType,
    sflsBlockDeviceMappings,
    sflsPlacement,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    mkSpotFleetMonitoring,
    sfmEnabled,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    mkSpotFleetRequestConfig,
    sfrcSpotFleetRequestConfig,
    sfrcSpotFleetRequestId,
    sfrcSpotFleetRequestState,
    sfrcCreateTime,
    sfrcTags,
    sfrcActivityStatus,

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    mkSpotFleetRequestConfigData,
    sfrcdClientToken,
    sfrcdInstanceInterruptionBehavior,
    sfrcdOnDemandMaxTotalPrice,
    sfrcdSpotPrice,
    sfrcdSpotMaintenanceStrategies,
    sfrcdLoadBalancersConfig,
    sfrcdExcessCapacityTerminationPolicy,
    sfrcdOnDemandTargetCapacity,
    sfrcdLaunchTemplateConfigs,
    sfrcdTagSpecifications,
    sfrcdValidUntil,
    sfrcdTerminateInstancesWithExpiration,
    sfrcdOnDemandAllocationStrategy,
    sfrcdInstancePoolsToUseCount,
    sfrcdFulfilledCapacity,
    sfrcdType,
    sfrcdValidFrom,
    sfrcdReplaceUnhealthyInstances,
    sfrcdLaunchSpecifications,
    sfrcdOnDemandFulfilledCapacity,
    sfrcdSpotMaxTotalPrice,
    sfrcdAllocationStrategy,
    sfrcdIAMFleetRole,
    sfrcdTargetCapacity,

    -- * SpotFleetTagSpecification
    SpotFleetTagSpecification (..),
    mkSpotFleetTagSpecification,
    sftsResourceType,
    sftsTags,

    -- * SpotInstanceRequest
    SpotInstanceRequest (..),
    mkSpotInstanceRequest,
    sirInstanceId,
    sirStatus,
    sirState,
    sirActualBlockHourlyPrice,
    sirBlockDurationMinutes,
    sirInstanceInterruptionBehavior,
    sirProductDescription,
    sirSpotPrice,
    sirLaunchSpecification,
    sirAvailabilityZoneGroup,
    sirLaunchedAvailabilityZone,
    sirValidUntil,
    sirLaunchGroup,
    sirFault,
    sirSpotInstanceRequestId,
    sirType,
    sirValidFrom,
    sirCreateTime,
    sirTags,

    -- * SpotInstanceStateFault
    SpotInstanceStateFault (..),
    mkSpotInstanceStateFault,
    sisfCode,
    sisfMessage,

    -- * SpotInstanceStatus
    SpotInstanceStatus (..),
    mkSpotInstanceStatus,
    sisUpdateTime,
    sisCode,
    sisMessage,

    -- * SpotMaintenanceStrategies
    SpotMaintenanceStrategies (..),
    mkSpotMaintenanceStrategies,
    smsCapacityRebalance,

    -- * SpotMarketOptions
    SpotMarketOptions (..),
    mkSpotMarketOptions,
    smoBlockDurationMinutes,
    smoInstanceInterruptionBehavior,
    smoValidUntil,
    smoSpotInstanceType,
    smoMaxPrice,

    -- * SpotOptions
    SpotOptions (..),
    mkSpotOptions,
    soInstanceInterruptionBehavior,
    soSingleAvailabilityZone,
    soMaxTotalPrice,
    soMinTargetCapacity,
    soInstancePoolsToUseCount,
    soMaintenanceStrategies,
    soSingleInstanceType,
    soAllocationStrategy,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    mkSpotOptionsRequest,
    sorInstanceInterruptionBehavior,
    sorSingleAvailabilityZone,
    sorMaxTotalPrice,
    sorMinTargetCapacity,
    sorInstancePoolsToUseCount,
    sorMaintenanceStrategies,
    sorSingleInstanceType,
    sorAllocationStrategy,

    -- * SpotPlacement
    SpotPlacement (..),
    mkSpotPlacement,
    spAvailabilityZone,
    spTenancy,
    spGroupName,

    -- * SpotPrice
    SpotPrice (..),
    mkSpotPrice,
    sProductDescription,
    sSpotPrice,
    sInstanceType,
    sAvailabilityZone,
    sTimestamp,

    -- * StaleIPPermission
    StaleIPPermission (..),
    mkStaleIPPermission,
    sipFromPort,
    sipUserIdGroupPairs,
    sipPrefixListIds,
    sipIPProtocol,
    sipToPort,
    sipIPRanges,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    mkStaleSecurityGroup,
    ssgVPCId,
    ssgGroupId,
    ssgGroupName,
    ssgStaleIPPermissionsEgress,
    ssgStaleIPPermissions,
    ssgDescription,

    -- * StateReason
    StateReason (..),
    mkStateReason,
    srCode,
    srMessage,

    -- * Storage
    Storage (..),
    mkStorage,
    sS3,

    -- * StorageLocation
    StorageLocation (..),
    mkStorageLocation,
    slBucket,
    slKey,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    subIPv6CidrBlockAssociationSet,
    subAvailabilityZoneId,
    subOutpostARN,
    subAssignIPv6AddressOnCreation,
    subSubnetARN,
    subOwnerId,
    subCustomerOwnedIPv4Pool,
    subMapCustomerOwnedIPOnLaunch,
    subMapPublicIPOnLaunch,
    subDefaultForAz,
    subTags,
    subAvailabilityZone,
    subAvailableIPAddressCount,
    subCidrBlock,
    subState,
    subSubnetId,
    subVPCId,

    -- * SubnetAssociation
    SubnetAssociation (..),
    mkSubnetAssociation,
    saState,
    saSubnetId,

    -- * SubnetCidrBlockState
    SubnetCidrBlockState (..),
    mkSubnetCidrBlockState,
    scbsState,
    scbsStatusMessage,

    -- * SubnetIPv6CidrBlockAssociation
    SubnetIPv6CidrBlockAssociation (..),
    mkSubnetIPv6CidrBlockAssociation,
    sicbaAssociationId,
    sicbaIPv6CidrBlock,
    sicbaIPv6CidrBlockState,

    -- * SuccessfulInstanceCreditSpecificationItem
    SuccessfulInstanceCreditSpecificationItem (..),
    mkSuccessfulInstanceCreditSpecificationItem,
    sicsiInstanceId,

    -- * SuccessfulQueuedPurchaseDeletion
    SuccessfulQueuedPurchaseDeletion (..),
    mkSuccessfulQueuedPurchaseDeletion,
    sqpdReservedInstancesId,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceId,
    tdResourceType,
    tdKey,
    tdValue,

    -- * TagSpecification
    TagSpecification (..),
    mkTagSpecification,
    tsResourceType,
    tsTags,

    -- * TargetCapacitySpecification
    TargetCapacitySpecification (..),
    mkTargetCapacitySpecification,
    tcsOnDemandTargetCapacity,
    tcsDefaultTargetCapacityType,
    tcsTotalTargetCapacity,
    tcsSpotTargetCapacity,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    mkTargetCapacitySpecificationRequest,
    tcsrOnDemandTargetCapacity,
    tcsrDefaultTargetCapacityType,
    tcsrSpotTargetCapacity,
    tcsrTotalTargetCapacity,

    -- * TargetConfiguration
    TargetConfiguration (..),
    mkTargetConfiguration,
    tcInstanceCount,
    tcOfferingId,

    -- * TargetConfigurationRequest
    TargetConfigurationRequest (..),
    mkTargetConfigurationRequest,
    tcrInstanceCount,
    tcrOfferingId,

    -- * TargetGroup
    TargetGroup (..),
    mkTargetGroup,
    tgARN,

    -- * TargetGroupsConfig
    TargetGroupsConfig (..),
    mkTargetGroupsConfig,
    tgcTargetGroups,

    -- * TargetNetwork
    TargetNetwork (..),
    mkTargetNetwork,
    tnAssociationId,
    tnStatus,
    tnSecurityGroups,
    tnTargetNetworkId,
    tnVPCId,
    tnClientVPNEndpointId,

    -- * TargetReservationValue
    TargetReservationValue (..),
    mkTargetReservationValue,
    trvReservationValue,
    trvTargetConfiguration,

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    mkTerminateConnectionStatus,
    tcsCurrentStatus,
    tcsConnectionId,
    tcsPreviousStatus,

    -- * TrafficMirrorFilter
    TrafficMirrorFilter (..),
    mkTrafficMirrorFilter,
    tmfTrafficMirrorFilterId,
    tmfIngressFilterRules,
    tmfNetworkServices,
    tmfEgressFilterRules,
    tmfDescription,
    tmfTags,

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    mkTrafficMirrorFilterRule,
    tmfrRuleNumber,
    tmfrTrafficDirection,
    tmfrRuleAction,
    tmfrProtocol,
    tmfrTrafficMirrorFilterId,
    tmfrTrafficMirrorFilterRuleId,
    tmfrDestinationPortRange,
    tmfrSourceCidrBlock,
    tmfrSourcePortRange,
    tmfrDescription,
    tmfrDestinationCidrBlock,

    -- * TrafficMirrorPortRange
    TrafficMirrorPortRange (..),
    mkTrafficMirrorPortRange,
    tmprFromPort,
    tmprToPort,

    -- * TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (..),
    mkTrafficMirrorPortRangeRequest,
    tmprrFromPort,
    tmprrToPort,

    -- * TrafficMirrorSession
    TrafficMirrorSession (..),
    mkTrafficMirrorSession,
    tmsTrafficMirrorTargetId,
    tmsNetworkInterfaceId,
    tmsTrafficMirrorFilterId,
    tmsPacketLength,
    tmsOwnerId,
    tmsTrafficMirrorSessionId,
    tmsVirtualNetworkId,
    tmsSessionNumber,
    tmsDescription,
    tmsTags,

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    mkTrafficMirrorTarget,
    tmtTrafficMirrorTargetId,
    tmtNetworkInterfaceId,
    tmtNetworkLoadBalancerARN,
    tmtOwnerId,
    tmtType,
    tmtDescription,
    tmtTags,

    -- * TransitGateway
    TransitGateway (..),
    mkTransitGateway,
    tgCreationTime,
    tgState,
    tgOwnerId,
    tgTransitGatewayARN,
    tgTransitGatewayId,
    tgOptions,
    tgDescription,
    tgTags,

    -- * TransitGatewayAssociation
    TransitGatewayAssociation (..),
    mkTransitGatewayAssociation,
    traState,
    traResourceId,
    traResourceType,
    traTransitGatewayRouteTableId,
    traTransitGatewayAttachmentId,

    -- * TransitGatewayAttachment
    TransitGatewayAttachment (..),
    mkTransitGatewayAttachment,
    tgaCreationTime,
    tgaState,
    tgaResourceId,
    tgaResourceType,
    tgaTransitGatewayOwnerId,
    tgaTransitGatewayId,
    tgaTransitGatewayAttachmentId,
    tgaResourceOwnerId,
    tgaTags,
    tgaAssociation,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    mkTransitGatewayAttachmentAssociation,
    tgaaState,
    tgaaTransitGatewayRouteTableId,

    -- * TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    mkTransitGatewayAttachmentPropagation,
    tgapState,
    tgapTransitGatewayRouteTableId,

    -- * TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    mkTransitGatewayMulticastDeregisteredGroupMembers,
    tgmdgmDeregisteredNetworkInterfaceIds,
    tgmdgmTransitGatewayMulticastDomainId,
    tgmdgmGroupIPAddress,

    -- * TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    mkTransitGatewayMulticastDeregisteredGroupSources,
    tgmdgsDeregisteredNetworkInterfaceIds,
    tgmdgsTransitGatewayMulticastDomainId,
    tgmdgsGroupIPAddress,

    -- * TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    mkTransitGatewayMulticastDomain,
    tgmdCreationTime,
    tgmdState,
    tgmdTransitGatewayMulticastDomainId,
    tgmdTransitGatewayId,
    tgmdTags,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    mkTransitGatewayMulticastDomainAssociation,
    tgmdaResourceId,
    tgmdaResourceType,
    tgmdaSubnet,
    tgmdaTransitGatewayAttachmentId,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    mkTransitGatewayMulticastDomainAssociations,
    tResourceId,
    tResourceType,
    tSubnets,
    tTransitGatewayMulticastDomainId,
    tTransitGatewayAttachmentId,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    mkTransitGatewayMulticastGroup,
    tgmgResourceId,
    tgmgResourceType,
    tgmgSourceType,
    tgmgMemberType,
    tgmgNetworkInterfaceId,
    tgmgSubnetId,
    tgmgGroupMember,
    tgmgGroupSource,
    tgmgGroupIPAddress,
    tgmgTransitGatewayAttachmentId,

    -- * TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    mkTransitGatewayMulticastRegisteredGroupMembers,
    tgmrgmTransitGatewayMulticastDomainId,
    tgmrgmRegisteredNetworkInterfaceIds,
    tgmrgmGroupIPAddress,

    -- * TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    mkTransitGatewayMulticastRegisteredGroupSources,
    tgmrgsTransitGatewayMulticastDomainId,
    tgmrgsRegisteredNetworkInterfaceIds,
    tgmrgsGroupIPAddress,

    -- * TransitGatewayOptions
    TransitGatewayOptions (..),
    mkTransitGatewayOptions,
    tgoVPNEcmpSupport,
    tgoAutoAcceptSharedAttachments,
    tgoPropagationDefaultRouteTableId,
    tgoDefaultRouteTableAssociation,
    tgoAssociationDefaultRouteTableId,
    tgoAmazonSideASN,
    tgoDefaultRouteTablePropagation,
    tgoMulticastSupport,
    tgoDNSSupport,

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    mkTransitGatewayPeeringAttachment,
    tgpaCreationTime,
    tgpaRequesterTgwInfo,
    tgpaStatus,
    tgpaState,
    tgpaAccepterTgwInfo,
    tgpaTransitGatewayAttachmentId,
    tgpaTags,

    -- * TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (..),
    mkTransitGatewayPrefixListAttachment,
    tgplaResourceId,
    tgplaResourceType,
    tgplaTransitGatewayAttachmentId,

    -- * TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (..),
    mkTransitGatewayPrefixListReference,
    tgplrState,
    tgplrTransitGatewayRouteTableId,
    tgplrPrefixListOwnerId,
    tgplrBlackhole,
    tgplrPrefixListId,
    tgplrTransitGatewayAttachment,

    -- * TransitGatewayPropagation
    TransitGatewayPropagation (..),
    mkTransitGatewayPropagation,
    tgpState,
    tgpResourceId,
    tgpResourceType,
    tgpTransitGatewayRouteTableId,
    tgpTransitGatewayAttachmentId,

    -- * TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    mkTransitGatewayRequestOptions,
    tgroVPNEcmpSupport,
    tgroAutoAcceptSharedAttachments,
    tgroDefaultRouteTableAssociation,
    tgroAmazonSideASN,
    tgroDefaultRouteTablePropagation,
    tgroMulticastSupport,
    tgroDNSSupport,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    mkTransitGatewayRoute,
    tgrState,
    tgrPrefixListId,
    tgrTransitGatewayAttachments,
    tgrType,
    tgrDestinationCidrBlock,

    -- * TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (..),
    mkTransitGatewayRouteAttachment,
    tgraResourceId,
    tgraResourceType,
    tgraTransitGatewayAttachmentId,

    -- * TransitGatewayRouteTable
    TransitGatewayRouteTable (..),
    mkTransitGatewayRouteTable,
    tgrtCreationTime,
    tgrtState,
    tgrtDefaultPropagationRouteTable,
    tgrtTransitGatewayRouteTableId,
    tgrtTransitGatewayId,
    tgrtDefaultAssociationRouteTable,
    tgrtTags,

    -- * TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (..),
    mkTransitGatewayRouteTableAssociation,
    tgrtaState,
    tgrtaResourceId,
    tgrtaResourceType,
    tgrtaTransitGatewayAttachmentId,

    -- * TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (..),
    mkTransitGatewayRouteTablePropagation,
    tgrtpState,
    tgrtpResourceId,
    tgrtpResourceType,
    tgrtpTransitGatewayAttachmentId,

    -- * TransitGatewayVPCAttachment
    TransitGatewayVPCAttachment (..),
    mkTransitGatewayVPCAttachment,
    tgvaCreationTime,
    tgvaState,
    tgvaSubnetIds,
    tgvaVPCId,
    tgvaTransitGatewayId,
    tgvaOptions,
    tgvaTransitGatewayAttachmentId,
    tgvaTags,
    tgvaVPCOwnerId,

    -- * TransitGatewayVPCAttachmentOptions
    TransitGatewayVPCAttachmentOptions (..),
    mkTransitGatewayVPCAttachmentOptions,
    tgvaoIPv6Support,
    tgvaoApplianceModeSupport,
    tgvaoDNSSupport,

    -- * TunnelOption
    TunnelOption (..),
    mkTunnelOption,
    toOutsideIPAddress,
    toReplayWindowSize,
    toDpdTimeoutAction,
    toRekeyFuzzPercentage,
    toPhase1LifetimeSeconds,
    toIkeVersions,
    toPhase2IntegrityAlgorithms,
    toPhase2LifetimeSeconds,
    toPhase1EncryptionAlgorithms,
    toPhase1DHGroupNumbers,
    toPhase1IntegrityAlgorithms,
    toRekeyMarginTimeSeconds,
    toDpdTimeoutSeconds,
    toTunnelInsideCidr,
    toStartupAction,
    toPhase2EncryptionAlgorithms,
    toPhase2DHGroupNumbers,
    toPreSharedKey,
    toTunnelInsideIPv6Cidr,

    -- * UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    mkUnsuccessfulInstanceCreditSpecificationItem,
    uicsiInstanceId,
    uicsiError,

    -- * UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    mkUnsuccessfulInstanceCreditSpecificationItemError,
    uicsieCode,
    uicsieMessage,

    -- * UnsuccessfulItem
    UnsuccessfulItem (..),
    mkUnsuccessfulItem,
    uiResourceId,
    uiError,

    -- * UnsuccessfulItemError
    UnsuccessfulItemError (..),
    mkUnsuccessfulItemError,
    uieCode,
    uieMessage,

    -- * UserBucket
    UserBucket (..),
    mkUserBucket,
    ubS3Key,
    ubS3Bucket,

    -- * UserBucketDetails
    UserBucketDetails (..),
    mkUserBucketDetails,
    ubdS3Key,
    ubdS3Bucket,

    -- * UserData
    UserData (..),
    mkUserData,
    udData,

    -- * UserIdGroupPair
    UserIdGroupPair (..),
    mkUserIdGroupPair,
    uigpVPCPeeringConnectionId,
    uigpVPCId,
    uigpUserId,
    uigpGroupId,
    uigpGroupName,
    uigpDescription,
    uigpPeeringStatus,

    -- * VCPUInfo
    VCPUInfo (..),
    mkVCPUInfo,
    vciValidThreadsPerCore,
    vciDefaultThreadsPerCore,
    vciDefaultVCPUs,
    vciDefaultCores,
    vciValidCores,

    -- * VGWTelemetry
    VGWTelemetry (..),
    mkVGWTelemetry,
    vtStatus,
    vtOutsideIPAddress,
    vtCertificateARN,
    vtLastStatusChange,
    vtAcceptedRouteCount,
    vtStatusMessage,

    -- * VPC
    VPC (..),
    mkVPC,
    vpcIPv6CidrBlockAssociationSet,
    vpcCidrBlockAssociationSet,
    vpcOwnerId,
    vpcTags,
    vpcIsDefault,
    vpcCidrBlock,
    vpcDHCPOptionsId,
    vpcInstanceTenancy,
    vpcState,
    vpcVPCId,

    -- * VPCAttachment
    VPCAttachment (..),
    mkVPCAttachment,
    vaState,
    vaVPCId,

    -- * VPCCidrBlockAssociation
    VPCCidrBlockAssociation (..),
    mkVPCCidrBlockAssociation,
    vcbaAssociationId,
    vcbaCidrBlockState,
    vcbaCidrBlock,

    -- * VPCCidrBlockState
    VPCCidrBlockState (..),
    mkVPCCidrBlockState,
    vcbsState,
    vcbsStatusMessage,

    -- * VPCClassicLink
    VPCClassicLink (..),
    mkVPCClassicLink,
    vclVPCId,
    vclTags,
    vclClassicLinkEnabled,

    -- * VPCEndpoint
    VPCEndpoint (..),
    mkVPCEndpoint,
    veGroups,
    veState,
    vePolicyDocument,
    veSubnetIds,
    veNetworkInterfaceIds,
    veVPCId,
    veRequesterManaged,
    veDNSEntries,
    veVPCEndpointType,
    vePrivateDNSEnabled,
    veOwnerId,
    veCreationTimestamp,
    veServiceName,
    veLastError,
    veVPCEndpointId,
    veTags,
    veRouteTableIds,

    -- * VPCEndpointConnection
    VPCEndpointConnection (..),
    mkVPCEndpointConnection,
    vecVPCEndpointOwner,
    vecNetworkLoadBalancerARNs,
    vecDNSEntries,
    vecVPCEndpointState,
    vecGatewayLoadBalancerARNs,
    vecCreationTimestamp,
    vecServiceId,
    vecVPCEndpointId,

    -- * VPCIPv6CidrBlockAssociation
    VPCIPv6CidrBlockAssociation (..),
    mkVPCIPv6CidrBlockAssociation,
    vicbaAssociationId,
    vicbaIPv6CidrBlock,
    vicbaNetworkBorderGroup,
    vicbaIPv6CidrBlockState,
    vicbaIPv6Pool,

    -- * VPCPeeringConnection
    VPCPeeringConnection (..),
    mkVPCPeeringConnection,
    vpcpcVPCPeeringConnectionId,
    vpcpcStatus,
    vpcpcAccepterVPCInfo,
    vpcpcRequesterVPCInfo,
    vpcpcExpirationTime,
    vpcpcTags,

    -- * VPCPeeringConnectionOptionsDescription
    VPCPeeringConnectionOptionsDescription (..),
    mkVPCPeeringConnectionOptionsDescription,
    vpcodAllowEgressFromLocalVPCToRemoteClassicLink,
    vpcodAllowEgressFromLocalClassicLinkToRemoteVPC,
    vpcodAllowDNSResolutionFromRemoteVPC,

    -- * VPCPeeringConnectionStateReason
    VPCPeeringConnectionStateReason (..),
    mkVPCPeeringConnectionStateReason,
    vpcsrCode,
    vpcsrMessage,

    -- * VPCPeeringConnectionVPCInfo
    VPCPeeringConnectionVPCInfo (..),
    mkVPCPeeringConnectionVPCInfo,
    vpcviCidrBlockSet,
    vpcviVPCId,
    vpcviOwnerId,
    vpcviPeeringOptions,
    vpcviCidrBlock,
    vpcviRegion,
    vpcviIPv6CidrBlockSet,

    -- * VPNConnection
    VPNConnection (..),
    mkVPNConnection,
    vcCustomerGatewayConfiguration,
    vcRoutes,
    vcVPNGatewayId,
    vcCategory,
    vcTransitGatewayId,
    vcOptions,
    vcTags,
    vcVGWTelemetry,
    vcVPNConnectionId,
    vcCustomerGatewayId,
    vcState,
    vcType,

    -- * VPNConnectionOptions
    VPNConnectionOptions (..),
    mkVPNConnectionOptions,
    vcoTunnelInsideIPVersion,
    vcoRemoteIPv4NetworkCidr,
    vcoEnableAcceleration,
    vcoLocalIPv4NetworkCidr,
    vcoRemoteIPv6NetworkCidr,
    vcoTunnelOptions,
    vcoLocalIPv6NetworkCidr,
    vcoStaticRoutesOnly,

    -- * VPNConnectionOptionsSpecification
    VPNConnectionOptionsSpecification (..),
    mkVPNConnectionOptionsSpecification,
    vcosTunnelInsideIPVersion,
    vcosRemoteIPv4NetworkCidr,
    vcosEnableAcceleration,
    vcosLocalIPv4NetworkCidr,
    vcosRemoteIPv6NetworkCidr,
    vcosTunnelOptions,
    vcosLocalIPv6NetworkCidr,
    vcosStaticRoutesOnly,

    -- * VPNGateway
    VPNGateway (..),
    mkVPNGateway,
    vgState,
    vgVPCAttachments,
    vgVPNGatewayId,
    vgAmazonSideASN,
    vgAvailabilityZone,
    vgType,
    vgTags,

    -- * VPNStaticRoute
    VPNStaticRoute (..),
    mkVPNStaticRoute,
    vsrState,
    vsrSource,
    vsrDestinationCidrBlock,

    -- * VPNTunnelOptionsSpecification
    VPNTunnelOptionsSpecification (..),
    mkVPNTunnelOptionsSpecification,
    vtosReplayWindowSize,
    vtosDPDTimeoutAction,
    vtosRekeyFuzzPercentage,
    vtosPhase1LifetimeSeconds,
    vtosIKEVersions,
    vtosPhase2IntegrityAlgorithms,
    vtosPhase2LifetimeSeconds,
    vtosPhase1EncryptionAlgorithms,
    vtosPhase1DHGroupNumbers,
    vtosPhase1IntegrityAlgorithms,
    vtosRekeyMarginTimeSeconds,
    vtosDPDTimeoutSeconds,
    vtosTunnelInsideCidr,
    vtosStartupAction,
    vtosPhase2EncryptionAlgorithms,
    vtosPhase2DHGroupNumbers,
    vtosPreSharedKey,
    vtosTunnelInsideIPv6Cidr,

    -- * ValidationError
    ValidationError (..),
    mkValidationError,
    veCode,
    veMessage,

    -- * ValidationWarning
    ValidationWarning (..),
    mkValidationWarning,
    vwErrors,

    -- * Volume
    Volume (..),
    mkVolume,
    vFastRestored,
    vMultiAttachEnabled,
    vAttachments,
    vIOPS,
    vOutpostARN,
    vKMSKeyId,
    vTags,
    vAvailabilityZone,
    vCreateTime,
    vEncrypted,
    vSize,
    vSnapshotId,
    vState,
    vVolumeId,
    vVolumeType,

    -- * VolumeAttachment
    VolumeAttachment (..),
    mkVolumeAttachment,
    volInstanceId,
    volDeleteOnTermination,
    volState,
    volDevice,
    volVolumeId,
    volAttachTime,

    -- * VolumeDetail
    VolumeDetail (..),
    mkVolumeDetail,
    vdSize,

    -- * VolumeModification
    VolumeModification (..),
    mkVolumeModification,
    vmProgress,
    vmStartTime,
    vmModificationState,
    vmTargetVolumeType,
    vmOriginalVolumeType,
    vmTargetSize,
    vmTargetIOPS,
    vmOriginalSize,
    vmOriginalIOPS,
    vmStatusMessage,
    vmEndTime,
    vmVolumeId,

    -- * VolumeStatusAction
    VolumeStatusAction (..),
    mkVolumeStatusAction,
    vsaEventType,
    vsaCode,
    vsaDescription,
    vsaEventId,

    -- * VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    mkVolumeStatusAttachmentStatus,
    vsasInstanceId,
    vsasIOPerformance,

    -- * VolumeStatusDetails
    VolumeStatusDetails (..),
    mkVolumeStatusDetails,
    vsdStatus,
    vsdName,

    -- * VolumeStatusEvent
    VolumeStatusEvent (..),
    mkVolumeStatusEvent,
    vseInstanceId,
    vseNotBefore,
    vseEventType,
    vseDescription,
    vseNotAfter,
    vseEventId,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    mkVolumeStatusInfo,
    vsiStatus,
    vsiDetails,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    mkVolumeStatusItem,
    vsiVolumeStatus,
    vsiActions,
    vsiOutpostARN,
    vsiEvents,
    vsiAvailabilityZone,
    vsiVolumeId,
    vsiAttachmentStatuses,
  )
where

import Network.AWS.EC2.Types.AccountAttribute
import Network.AWS.EC2.Types.AccountAttributeName
import Network.AWS.EC2.Types.AccountAttributeValue
import Network.AWS.EC2.Types.ActiveInstance
import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.AddPrefixListEntry
import Network.AWS.EC2.Types.Address
import Network.AWS.EC2.Types.AddressStatus
import Network.AWS.EC2.Types.Affinity
import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllocationStrategy
import Network.AWS.EC2.Types.AllowedPrincipal
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.ArchitectureType
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.AssignedPrivateIPAddress
import Network.AWS.EC2.Types.AssociatedNetworkType
import Network.AWS.EC2.Types.AssociatedRole
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.AssociationStatus
import Network.AWS.EC2.Types.AssociationStatusCode
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.EC2.Types.AttributeBooleanValue
import Network.AWS.EC2.Types.AttributeValue
import Network.AWS.EC2.Types.AuthorizationRule
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailabilityZone
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.BlobAttributeValue
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.BundleTask
import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.BundleTaskState
import Network.AWS.EC2.Types.ByoipCidr
import Network.AWS.EC2.Types.ByoipCidrState
import Network.AWS.EC2.Types.CPUOptions
import Network.AWS.EC2.Types.CPUOptionsRequest
import Network.AWS.EC2.Types.CancelBatchErrorCode
import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
import Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Network.AWS.EC2.Types.CancelSpotInstanceRequestState
import Network.AWS.EC2.Types.CancelledSpotInstanceRequest
import Network.AWS.EC2.Types.CapacityReservation
import Network.AWS.EC2.Types.CapacityReservationGroup
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
import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.CidrAuthorizationContext
import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.ClassicLinkDNSSupport
import Network.AWS.EC2.Types.ClassicLinkInstance
import Network.AWS.EC2.Types.ClassicLoadBalancer
import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
import Network.AWS.EC2.Types.ClientConnectOptions
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientData
import Network.AWS.EC2.Types.ClientVPNAuthentication
import Network.AWS.EC2.Types.ClientVPNAuthenticationRequest
import Network.AWS.EC2.Types.ClientVPNAuthenticationType
import Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatus
import Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode
import Network.AWS.EC2.Types.ClientVPNConnection
import Network.AWS.EC2.Types.ClientVPNConnectionStatus
import Network.AWS.EC2.Types.ClientVPNConnectionStatusCode
import Network.AWS.EC2.Types.ClientVPNEndpoint
import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatus
import Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatusCode
import Network.AWS.EC2.Types.ClientVPNEndpointStatus
import Network.AWS.EC2.Types.ClientVPNEndpointStatusCode
import Network.AWS.EC2.Types.ClientVPNRoute
import Network.AWS.EC2.Types.ClientVPNRouteStatus
import Network.AWS.EC2.Types.ClientVPNRouteStatusCode
import Network.AWS.EC2.Types.CoipAddressUsage
import Network.AWS.EC2.Types.CoipPool
import Network.AWS.EC2.Types.ConnectionLogOptions
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.ConnectionNotification
import Network.AWS.EC2.Types.ConnectionNotificationState
import Network.AWS.EC2.Types.ConnectionNotificationType
import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.ConversionTask
import Network.AWS.EC2.Types.ConversionTaskState
import Network.AWS.EC2.Types.CopyTagsFromSource
import Network.AWS.EC2.Types.CreateFleetError
import Network.AWS.EC2.Types.CreateFleetInstance
import Network.AWS.EC2.Types.CreateTransitGatewayVPCAttachmentRequestOptions
import Network.AWS.EC2.Types.CreateVolumePermission
import Network.AWS.EC2.Types.CreateVolumePermissionModifications
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.CustomerGateway
import Network.AWS.EC2.Types.DHCPConfiguration
import Network.AWS.EC2.Types.DHCPOptions
import Network.AWS.EC2.Types.DNSEntry
import Network.AWS.EC2.Types.DNSNameState
import Network.AWS.EC2.Types.DNSServersOptionsModifyStructure
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.DatafeedSubscriptionState
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.DefaultTargetCapacityType
import Network.AWS.EC2.Types.DeleteFleetError
import Network.AWS.EC2.Types.DeleteFleetErrorCode
import Network.AWS.EC2.Types.DeleteFleetErrorItem
import Network.AWS.EC2.Types.DeleteFleetSuccessItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
import Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.DescribeFleetError
import Network.AWS.EC2.Types.DescribeFleetsInstances
import Network.AWS.EC2.Types.DeviceType
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
import Network.AWS.EC2.Types.DomainType
import Network.AWS.EC2.Types.EBSBlockDevice
import Network.AWS.EC2.Types.EBSEncryptionSupport
import Network.AWS.EC2.Types.EBSInfo
import Network.AWS.EC2.Types.EBSInstanceBlockDevice
import Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification
import Network.AWS.EC2.Types.EBSNvmeSupport
import Network.AWS.EC2.Types.EBSOptimizedInfo
import Network.AWS.EC2.Types.EBSOptimizedSupport
import Network.AWS.EC2.Types.EgressOnlyInternetGateway
import Network.AWS.EC2.Types.ElasticGpuAssociation
import Network.AWS.EC2.Types.ElasticGpuHealth
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
import Network.AWS.EC2.Types.EndDateType
import Network.AWS.EC2.Types.EphemeralNvmeSupport
import Network.AWS.EC2.Types.EventCode
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.EventType
import Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.ExportEnvironment
import Network.AWS.EC2.Types.ExportImageTask
import Network.AWS.EC2.Types.ExportTask
import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.ExportTaskS3LocationRequest
import Network.AWS.EC2.Types.ExportTaskState
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.ExportToS3TaskSpecification
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
import Network.AWS.EC2.Types.FlowLogsResourceType
import Network.AWS.EC2.Types.FpgaDeviceInfo
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import Network.AWS.EC2.Types.FpgaImage
import Network.AWS.EC2.Types.FpgaImageAttribute
import Network.AWS.EC2.Types.FpgaImageAttributeName
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.FpgaImageStateCode
import Network.AWS.EC2.Types.FpgaInfo
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.GpuDeviceInfo
import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import Network.AWS.EC2.Types.GpuInfo
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.HTTPTokensState
import Network.AWS.EC2.Types.HibernationOptions
import Network.AWS.EC2.Types.HibernationOptionsRequest
import Network.AWS.EC2.Types.HistoryRecord
import Network.AWS.EC2.Types.HistoryRecordEntry
import Network.AWS.EC2.Types.Host
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostOffering
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostRecovery
import Network.AWS.EC2.Types.HostReservation
import Network.AWS.EC2.Types.HostTenancy
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.IAMInstanceProfile
import Network.AWS.EC2.Types.IAMInstanceProfileAssociation
import Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
import Network.AWS.EC2.Types.IAMInstanceProfileSpecification
import Network.AWS.EC2.Types.ICMPTypeCode
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.IPPermission
import Network.AWS.EC2.Types.IPRange
import Network.AWS.EC2.Types.IPv6CidrAssociation
import Network.AWS.EC2.Types.IPv6CidrBlock
import Network.AWS.EC2.Types.IPv6Pool
import Network.AWS.EC2.Types.IPv6Range
import Network.AWS.EC2.Types.IPv6SupportValue
import Network.AWS.EC2.Types.IdFormat
import Network.AWS.EC2.Types.Image
import Network.AWS.EC2.Types.ImageAttributeName
import Network.AWS.EC2.Types.ImageDiskContainer
import Network.AWS.EC2.Types.ImageState
import Network.AWS.EC2.Types.ImageTypeValues
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.ImportImageTask
import Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.ImportSnapshotTask
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.InferenceAcceleratorInfo
import Network.AWS.EC2.Types.InferenceDeviceInfo
import Network.AWS.EC2.Types.Instance
import Network.AWS.EC2.Types.InstanceAttributeName
import Network.AWS.EC2.Types.InstanceBlockDeviceMapping
import Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
import Network.AWS.EC2.Types.InstanceCapacity
import Network.AWS.EC2.Types.InstanceCount
import Network.AWS.EC2.Types.InstanceCreditSpecification
import Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
import Network.AWS.EC2.Types.InstanceHealthStatus
import Network.AWS.EC2.Types.InstanceIPv6Address
import Network.AWS.EC2.Types.InstanceIPv6AddressRequest
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
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
import Network.AWS.EC2.Types.InstancePrivateIPAddress
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
import Network.AWS.EC2.Types.InternetGateway
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.KeyPairInfo
import Network.AWS.EC2.Types.LastError
import Network.AWS.EC2.Types.LaunchPermission
import Network.AWS.EC2.Types.LaunchPermissionModifications
import Network.AWS.EC2.Types.LaunchSpecification
import Network.AWS.EC2.Types.LaunchTemplate
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCPUOptions
import Network.AWS.EC2.Types.LaunchTemplateCPUOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.LaunchTemplateConfig
import Network.AWS.EC2.Types.LaunchTemplateEBSBlockDevice
import Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateErrorCode
import Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification
import Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
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
import Network.AWS.EC2.Types.LocalGatewayRoute
import Network.AWS.EC2.Types.LocalGatewayRouteState
import Network.AWS.EC2.Types.LocalGatewayRouteTable
import Network.AWS.EC2.Types.LocalGatewayRouteTableVPCAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteType
import Network.AWS.EC2.Types.LocalGatewayVirtualInterface
import Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Network.AWS.EC2.Types.LocationType
import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.ManagedPrefixList
import Network.AWS.EC2.Types.MarketType
import Network.AWS.EC2.Types.MembershipType
import Network.AWS.EC2.Types.MemoryInfo
import Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.ModifyTransitGatewayOptions
import Network.AWS.EC2.Types.ModifyTransitGatewayVPCAttachmentRequestOptions
import Network.AWS.EC2.Types.ModifyVPNTunnelOptionsSpecification
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.EC2.Types.MonitoringState
import Network.AWS.EC2.Types.MoveStatus
import Network.AWS.EC2.Types.MovingAddressStatus
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.NatGateway
import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NatGatewayState
import Network.AWS.EC2.Types.NetworkACL
import Network.AWS.EC2.Types.NetworkACLAssociation
import Network.AWS.EC2.Types.NetworkACLEntry
import Network.AWS.EC2.Types.NetworkCardInfo
import Network.AWS.EC2.Types.NetworkInfo
import Network.AWS.EC2.Types.NetworkInterface
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
import Network.AWS.EC2.Types.NetworkInterfaceAttribute
import Network.AWS.EC2.Types.NetworkInterfaceCreationType
import Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
import Network.AWS.EC2.Types.NetworkInterfacePermission
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
import Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.NewDHCPConfiguration
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.OnDemandAllocationStrategy
import Network.AWS.EC2.Types.OnDemandOptions
import Network.AWS.EC2.Types.OnDemandOptionsRequest
import Network.AWS.EC2.Types.OperationType
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.EC2.Types.PciId
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
import Network.AWS.EC2.Types.PlacementGroupInfo
import Network.AWS.EC2.Types.PlacementGroupState
import Network.AWS.EC2.Types.PlacementGroupStrategy
import Network.AWS.EC2.Types.PlacementResponse
import Network.AWS.EC2.Types.PlacementStrategy
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.PrefixList
import Network.AWS.EC2.Types.PrefixListAssociation
import Network.AWS.EC2.Types.PrefixListEntry
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.PrefixListState
import Network.AWS.EC2.Types.PriceSchedule
import Network.AWS.EC2.Types.PriceScheduleSpecification
import Network.AWS.EC2.Types.PricingDetail
import Network.AWS.EC2.Types.PrincipalIdFormat
import Network.AWS.EC2.Types.PrincipalType
import Network.AWS.EC2.Types.PrivateDNSDetails
import Network.AWS.EC2.Types.PrivateDNSNameConfiguration
import Network.AWS.EC2.Types.PrivateIPAddressSpecification
import Network.AWS.EC2.Types.ProcessorInfo
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.ProductCodeValues
import Network.AWS.EC2.Types.PropagatingVGW
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.PublicIPv4Pool
import Network.AWS.EC2.Types.PublicIPv4PoolRange
import Network.AWS.EC2.Types.Purchase
import Network.AWS.EC2.Types.PurchaseRequest
import Network.AWS.EC2.Types.RIProductDescription
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
import Network.AWS.EC2.Types.ReservationState
import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.EC2.Types.ReservedInstanceLimitPrice
import Network.AWS.EC2.Types.ReservedInstanceReservationValue
import Network.AWS.EC2.Types.ReservedInstanceState
import Network.AWS.EC2.Types.ReservedInstances
import Network.AWS.EC2.Types.ReservedInstancesConfiguration
import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesListing
import Network.AWS.EC2.Types.ReservedInstancesModification
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import Network.AWS.EC2.Types.ReservedInstancesOffering
import Network.AWS.EC2.Types.ResetFpgaImageAttributeName
import Network.AWS.EC2.Types.ResetImageAttributeName
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.ResponseError
import Network.AWS.EC2.Types.ResponseLaunchTemplateData
import Network.AWS.EC2.Types.RootDeviceType
import Network.AWS.EC2.Types.Route
import Network.AWS.EC2.Types.RouteOrigin
import Network.AWS.EC2.Types.RouteState
import Network.AWS.EC2.Types.RouteTable
import Network.AWS.EC2.Types.RouteTableAssociation
import Network.AWS.EC2.Types.RouteTableAssociationState
import Network.AWS.EC2.Types.RouteTableAssociationStateCode
import Network.AWS.EC2.Types.RuleAction
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.S3Storage
import Network.AWS.EC2.Types.ScheduledInstance
import Network.AWS.EC2.Types.ScheduledInstanceAvailability
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
import Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Network.AWS.EC2.Types.ScheduledInstancesEBS
import Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
import Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
import Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
import Network.AWS.EC2.Types.ScheduledInstancesMonitoring
import Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
import Network.AWS.EC2.Types.ScheduledInstancesPlacement
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
import Network.AWS.EC2.Types.Scope
import Network.AWS.EC2.Types.SecurityGroup
import Network.AWS.EC2.Types.SecurityGroupIdentifier
import Network.AWS.EC2.Types.SecurityGroupReference
import Network.AWS.EC2.Types.SelfServicePortal
import Network.AWS.EC2.Types.ServiceConfiguration
import Network.AWS.EC2.Types.ServiceDetail
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
import Network.AWS.EC2.Types.SnapshotInfo
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotCapacityRebalance
import Network.AWS.EC2.Types.SpotDatafeedSubscription
import Network.AWS.EC2.Types.SpotFleetLaunchSpecification
import Network.AWS.EC2.Types.SpotFleetMonitoring
import Network.AWS.EC2.Types.SpotFleetRequestConfig
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.SpotFleetTagSpecification
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceRequest
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
import Network.AWS.EC2.Types.StaleIPPermission
import Network.AWS.EC2.Types.StaleSecurityGroup
import Network.AWS.EC2.Types.State
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.StatusName
import Network.AWS.EC2.Types.StatusType
import Network.AWS.EC2.Types.Storage
import Network.AWS.EC2.Types.StorageLocation
import Network.AWS.EC2.Types.Subnet
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.SubnetCidrBlockState
import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import Network.AWS.EC2.Types.SubnetIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
import Network.AWS.EC2.Types.SummaryStatus
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TagDescription
import Network.AWS.EC2.Types.TagSpecification
import Network.AWS.EC2.Types.TargetCapacitySpecification
import Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
import Network.AWS.EC2.Types.TargetConfiguration
import Network.AWS.EC2.Types.TargetConfigurationRequest
import Network.AWS.EC2.Types.TargetGroup
import Network.AWS.EC2.Types.TargetGroupsConfig
import Network.AWS.EC2.Types.TargetNetwork
import Network.AWS.EC2.Types.TargetReservationValue
import Network.AWS.EC2.Types.TelemetryStatus
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.TerminateConnectionStatus
import Network.AWS.EC2.Types.TrafficDirection
import Network.AWS.EC2.Types.TrafficMirrorFilter
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
import Network.AWS.EC2.Types.TrafficMirrorNetworkService
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
import Network.AWS.EC2.Types.TrafficMirrorRuleAction
import Network.AWS.EC2.Types.TrafficMirrorSession
import Network.AWS.EC2.Types.TrafficMirrorSessionField
import Network.AWS.EC2.Types.TrafficMirrorTarget
import Network.AWS.EC2.Types.TrafficMirrorTargetType
import Network.AWS.EC2.Types.TrafficType
import Network.AWS.EC2.Types.TransitGateway
import Network.AWS.EC2.Types.TransitGatewayAssociation
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachment
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayMulticastDomain
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
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
import Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
import Network.AWS.EC2.Types.TransitGatewayRouteTableState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import Network.AWS.EC2.Types.TransitGatewayState
import Network.AWS.EC2.Types.TransitGatewayVPCAttachment
import Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.TunnelInsideIPVersion
import Network.AWS.EC2.Types.TunnelOption
import Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Network.AWS.EC2.Types.UnsuccessfulItem
import Network.AWS.EC2.Types.UnsuccessfulItemError
import Network.AWS.EC2.Types.UsageClassType
import Network.AWS.EC2.Types.UserBucket
import Network.AWS.EC2.Types.UserBucketDetails
import Network.AWS.EC2.Types.UserData
import Network.AWS.EC2.Types.UserIdGroupPair
import Network.AWS.EC2.Types.VCPUInfo
import Network.AWS.EC2.Types.VGWTelemetry
import Network.AWS.EC2.Types.VPC
import Network.AWS.EC2.Types.VPCAttachment
import Network.AWS.EC2.Types.VPCAttributeName
import Network.AWS.EC2.Types.VPCCidrBlockAssociation
import Network.AWS.EC2.Types.VPCCidrBlockState
import Network.AWS.EC2.Types.VPCCidrBlockStateCode
import Network.AWS.EC2.Types.VPCClassicLink
import Network.AWS.EC2.Types.VPCEndpoint
import Network.AWS.EC2.Types.VPCEndpointConnection
import Network.AWS.EC2.Types.VPCEndpointType
import Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.VPCPeeringConnection
import Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
import Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
import Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
import Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
import Network.AWS.EC2.Types.VPCState
import Network.AWS.EC2.Types.VPCTenancy
import Network.AWS.EC2.Types.VPNConnection
import Network.AWS.EC2.Types.VPNConnectionOptions
import Network.AWS.EC2.Types.VPNConnectionOptionsSpecification
import Network.AWS.EC2.Types.VPNEcmpSupportValue
import Network.AWS.EC2.Types.VPNGateway
import Network.AWS.EC2.Types.VPNProtocol
import Network.AWS.EC2.Types.VPNState
import Network.AWS.EC2.Types.VPNStaticRoute
import Network.AWS.EC2.Types.VPNStaticRouteSource
import Network.AWS.EC2.Types.VPNTunnelOptionsSpecification
import Network.AWS.EC2.Types.ValidationError
import Network.AWS.EC2.Types.ValidationWarning
import Network.AWS.EC2.Types.VirtualizationType
import Network.AWS.EC2.Types.Volume
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeAttachmentState
import Network.AWS.EC2.Types.VolumeAttributeName
import Network.AWS.EC2.Types.VolumeDetail
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
ec2Service :: Lude.Service
ec2Service =
  Lude.Service
    { Lude._svcAbbrev = "EC2",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "ec2",
      Lude._svcVersion = "2016-11-15",
      Lude._svcEndpoint = Lude.defaultEndpoint ec2Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "EC2",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has
          (Lude.hasCode "RequestLimitExceeded" Lude.. Lude.hasStatus 503)
          e =
        Lude.Just "request_limit_exceeded"
      | Lens.has
          (Lude.hasCode "EC2ThrottledException" Lude.. Lude.hasStatus 503)
          e =
        Lude.Just "ec2_throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
