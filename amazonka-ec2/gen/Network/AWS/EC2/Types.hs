{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors

    -- * Re-exported Types
    module Network.AWS.EC2.Internal,

    -- * AccountAttributeName
    AccountAttributeName (..),

    -- * ActivityStatus
    ActivityStatus (..),

    -- * AddressAttributeName
    AddressAttributeName (..),

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

    -- * BatchState
    BatchState (..),

    -- * BgpStatus
    BgpStatus (..),

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

    -- * ContainerFormat
    ContainerFormat (..),

    -- * ConversionTaskState
    ConversionTaskState (..),

    -- * CopyTagsFromSource
    CopyTagsFromSource (..),

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

    -- * DeviceType
    DeviceType (..),

    -- * DiskImageFormat
    DiskImageFormat (..),

    -- * DiskType
    DiskType (..),

    -- * DnsNameState
    DnsNameState (..),

    -- * DnsSupportValue
    DnsSupportValue (..),

    -- * DomainType
    DomainType (..),

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

    -- * HostRecovery
    HostRecovery (..),

    -- * HostTenancy
    HostTenancy (..),

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

    -- * Ipv6SupportValue
    Ipv6SupportValue (..),

    -- * LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- * LaunchTemplateHttpTokensState
    LaunchTemplateHttpTokensState (..),

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

    -- * Protocol
    Protocol (..),

    -- * ProtocolValue
    ProtocolValue (..),

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

    -- * StaticSourcesSupportValue
    StaticSourcesSupportValue (..),

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

    -- * TransitGatewayConnectPeerState
    TransitGatewayConnectPeerState (..),

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
    activeInstance_instanceId,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,
    activeInstance_instanceHealth,

    -- * AddPrefixListEntry
    AddPrefixListEntry (..),
    newAddPrefixListEntry,
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- * Address
    Address (..),
    newAddress,
    address_customerOwnedIpv4Pool,
    address_instanceId,
    address_domain,
    address_carrierIp,
    address_customerOwnedIp,
    address_networkInterfaceOwnerId,
    address_associationId,
    address_tags,
    address_networkInterfaceId,
    address_publicIpv4Pool,
    address_publicIp,
    address_allocationId,
    address_networkBorderGroup,
    address_privateIpAddress,

    -- * AddressAttribute
    AddressAttribute (..),
    newAddressAttribute,
    addressAttribute_ptrRecord,
    addressAttribute_publicIp,
    addressAttribute_allocationId,
    addressAttribute_ptrRecordUpdate,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    newAllowedPrincipal,
    allowedPrincipal_principal,
    allowedPrincipal_principalType,

    -- * AlternatePathHint
    AlternatePathHint (..),
    newAlternatePathHint,
    alternatePathHint_componentId,
    alternatePathHint_componentArn,

    -- * AnalysisAclRule
    AnalysisAclRule (..),
    newAnalysisAclRule,
    analysisAclRule_portRange,
    analysisAclRule_ruleAction,
    analysisAclRule_egress,
    analysisAclRule_cidr,
    analysisAclRule_protocol,
    analysisAclRule_ruleNumber,

    -- * AnalysisComponent
    AnalysisComponent (..),
    newAnalysisComponent,
    analysisComponent_arn,
    analysisComponent_id,

    -- * AnalysisLoadBalancerListener
    AnalysisLoadBalancerListener (..),
    newAnalysisLoadBalancerListener,
    analysisLoadBalancerListener_loadBalancerPort,
    analysisLoadBalancerListener_instancePort,

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
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_protocol,
    analysisPacketHeader_sourcePortRanges,

    -- * AnalysisRouteTableRoute
    AnalysisRouteTableRoute (..),
    newAnalysisRouteTableRoute,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_transitGatewayId,

    -- * AnalysisSecurityGroupRule
    AnalysisSecurityGroupRule (..),
    newAnalysisSecurityGroupRule,
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_protocol,

    -- * AssignedPrivateIpAddress
    AssignedPrivateIpAddress (..),
    newAssignedPrivateIpAddress,
    assignedPrivateIpAddress_privateIpAddress,

    -- * AssociatedRole
    AssociatedRole (..),
    newAssociatedRole,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_certificateS3BucketName,
    associatedRole_associatedRoleArn,

    -- * AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    newAssociatedTargetNetwork,
    associatedTargetNetwork_networkType,
    associatedTargetNetwork_networkId,

    -- * AssociationStatus
    AssociationStatus (..),
    newAssociationStatus,
    associationStatus_message,
    associationStatus_code,

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
    authorizationRule_clientVpnEndpointId,
    authorizationRule_status,
    authorizationRule_destinationCidr,
    authorizationRule_accessAll,
    authorizationRule_groupId,
    authorizationRule_description,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_regionName,
    availabilityZone_parentZoneId,
    availabilityZone_zoneName,
    availabilityZone_zoneType,
    availabilityZone_zoneId,
    availabilityZone_groupName,
    availabilityZone_optInStatus,
    availabilityZone_state,
    availabilityZone_messages,
    availabilityZone_networkBorderGroup,
    availabilityZone_parentZoneName,

    -- * AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    newAvailabilityZoneMessage,
    availabilityZoneMessage_message,

    -- * AvailableCapacity
    AvailableCapacity (..),
    newAvailableCapacity,
    availableCapacity_availableInstanceCapacity,
    availableCapacity_availableVCpus,

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
    byoipCidr_statusMessage,
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_description,

    -- * CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    newCancelSpotFleetRequestsError,
    cancelSpotFleetRequestsError_message,
    cancelSpotFleetRequestsError_code,

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
    cancelledSpotInstanceRequest_state,
    cancelledSpotInstanceRequest_spotInstanceRequestId,

    -- * CapacityReservation
    CapacityReservation (..),
    newCapacityReservation,
    capacityReservation_ownerId,
    capacityReservation_startDate,
    capacityReservation_instanceType,
    capacityReservation_ebsOptimized,
    capacityReservation_endDateType,
    capacityReservation_availableInstanceCount,
    capacityReservation_createDate,
    capacityReservation_tenancy,
    capacityReservation_availabilityZoneId,
    capacityReservation_state,
    capacityReservation_availabilityZone,
    capacityReservation_capacityReservationId,
    capacityReservation_tags,
    capacityReservation_capacityReservationArn,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_ephemeralStorage,
    capacityReservation_instancePlatform,
    capacityReservation_endDate,
    capacityReservation_totalInstanceCount,

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
    capacityReservationTarget_capacityReservationResourceGroupArn,
    capacityReservationTarget_capacityReservationId,

    -- * CapacityReservationTargetResponse
    CapacityReservationTargetResponse (..),
    newCapacityReservationTargetResponse,
    capacityReservationTargetResponse_capacityReservationResourceGroupArn,
    capacityReservationTargetResponse_capacityReservationId,

    -- * CarrierGateway
    CarrierGateway (..),
    newCarrierGateway,
    carrierGateway_ownerId,
    carrierGateway_state,
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,
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
    clientCertificateRevocationListStatus_message,
    clientCertificateRevocationListStatus_code,

    -- * ClientConnectOptions
    ClientConnectOptions (..),
    newClientConnectOptions,
    clientConnectOptions_enabled,
    clientConnectOptions_lambdaFunctionArn,

    -- * ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    newClientConnectResponseOptions,
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,

    -- * ClientData
    ClientData (..),
    newClientData,
    clientData_uploadEnd,
    clientData_comment,
    clientData_uploadSize,
    clientData_uploadStart,

    -- * ClientVpnAuthentication
    ClientVpnAuthentication (..),
    newClientVpnAuthentication,
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_activeDirectory,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_type,

    -- * ClientVpnAuthenticationRequest
    ClientVpnAuthenticationRequest (..),
    newClientVpnAuthenticationRequest,
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_activeDirectory,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_type,

    -- * ClientVpnAuthorizationRuleStatus
    ClientVpnAuthorizationRuleStatus (..),
    newClientVpnAuthorizationRuleStatus,
    clientVpnAuthorizationRuleStatus_message,
    clientVpnAuthorizationRuleStatus_code,

    -- * ClientVpnConnection
    ClientVpnConnection (..),
    newClientVpnConnection,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_status,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_egressBytes,
    clientVpnConnection_connectionId,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_egressPackets,
    clientVpnConnection_timestamp,
    clientVpnConnection_username,
    clientVpnConnection_clientIp,

    -- * ClientVpnConnectionStatus
    ClientVpnConnectionStatus (..),
    newClientVpnConnectionStatus,
    clientVpnConnectionStatus_message,
    clientVpnConnectionStatus_code,

    -- * ClientVpnEndpoint
    ClientVpnEndpoint (..),
    newClientVpnEndpoint,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_status,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnProtocol,
    clientVpnEndpoint_description,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_splitTunnel,

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
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_status,
    clientVpnRoute_origin,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_description,
    clientVpnRoute_type,
    clientVpnRoute_targetSubnet,

    -- * ClientVpnRouteStatus
    ClientVpnRouteStatus (..),
    newClientVpnRouteStatus,
    clientVpnRouteStatus_message,
    clientVpnRouteStatus_code,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    newCoipAddressUsage,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,
    coipAddressUsage_allocationId,

    -- * CoipPool
    CoipPool (..),
    newCoipPool,
    coipPool_poolId,
    coipPool_poolArn,
    coipPool_poolCidrs,
    coipPool_localGatewayRouteTableId,
    coipPool_tags,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    newConnectionLogOptions,
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogGroup,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    newConnectionLogResponseOptions,
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogGroup,

    -- * ConnectionNotification
    ConnectionNotification (..),
    newConnectionNotification,
    connectionNotification_connectionEvents,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationType,
    connectionNotification_vpcEndpointId,
    connectionNotification_serviceId,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationArn,

    -- * ConversionTask
    ConversionTask (..),
    newConversionTask,
    conversionTask_statusMessage,
    conversionTask_importInstance,
    conversionTask_expirationTime,
    conversionTask_importVolume,
    conversionTask_state,
    conversionTask_tags,
    conversionTask_conversionTaskId,

    -- * CpuOptions
    CpuOptions (..),
    newCpuOptions,
    cpuOptions_threadsPerCore,
    cpuOptions_coreCount,

    -- * CpuOptionsRequest
    CpuOptionsRequest (..),
    newCpuOptionsRequest,
    cpuOptionsRequest_threadsPerCore,
    cpuOptionsRequest_coreCount,

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
    createFleetInstance_instanceIds,
    createFleetInstance_platform,
    createFleetInstance_instanceType,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,

    -- * CreateTransitGatewayConnectRequestOptions
    CreateTransitGatewayConnectRequestOptions (..),
    newCreateTransitGatewayConnectRequestOptions,
    createTransitGatewayConnectRequestOptions_protocol,

    -- * CreateTransitGatewayMulticastDomainRequestOptions
    CreateTransitGatewayMulticastDomainRequestOptions (..),
    newCreateTransitGatewayMulticastDomainRequestOptions,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,

    -- * CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (..),
    newCreateTransitGatewayVpcAttachmentRequestOptions,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

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
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
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
    deleteQueuedReservedInstancesError_message,
    deleteQueuedReservedInstancesError_code,

    -- * DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    newDeregisterInstanceTagAttributeRequest,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    newDescribeFastSnapshotRestoreSuccessItem,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_disabledTime,

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
    describeFleetsInstances_instanceIds,
    describeFleetsInstances_platform,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,

    -- * DhcpConfiguration
    DhcpConfiguration (..),
    newDhcpConfiguration,
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- * DhcpOptions
    DhcpOptions (..),
    newDhcpOptions,
    dhcpOptions_ownerId,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,
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
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_disabledTime,

    -- * DiskImage
    DiskImage (..),
    newDiskImage,
    diskImage_volume,
    diskImage_image,
    diskImage_description,

    -- * DiskImageDescription
    DiskImageDescription (..),
    newDiskImageDescription,
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,
    diskImageDescription_checksum,
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
    diskInfo_sizeInGB,
    diskInfo_count,
    diskInfo_type,

    -- * DnsEntry
    DnsEntry (..),
    newDnsEntry,
    dnsEntry_hostedZoneId,
    dnsEntry_dnsName,

    -- * DnsServersOptionsModifyStructure
    DnsServersOptionsModifyStructure (..),
    newDnsServersOptionsModifyStructure,
    dnsServersOptionsModifyStructure_enabled,
    dnsServersOptionsModifyStructure_customDnsServers,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_throughput,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,

    -- * EbsInfo
    EbsInfo (..),
    newEbsInfo,
    ebsInfo_nvmeSupport,
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    newEbsInstanceBlockDevice,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_deleteOnTermination,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_volumeId,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,

    -- * EbsOptimizedInfo
    EbsOptimizedInfo (..),
    newEbsOptimizedInfo,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    newEgressOnlyInternetGateway,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_tags,
    egressOnlyInternetGateway_attachments,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    newElasticGpuAssociation,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationState,
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
    elasticGpus_elasticGpuType,
    elasticGpus_instanceId,
    elasticGpus_elasticGpuHealth,
    elasticGpus_elasticGpuId,
    elasticGpus_availabilityZone,
    elasticGpus_tags,
    elasticGpus_elasticGpuState,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    newElasticInferenceAccelerator,
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    newElasticInferenceAcceleratorAssociation,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- * EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    newEnableFastSnapshotRestoreErrorItem,
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    enableFastSnapshotRestoreErrorItem_snapshotId,

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
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_disabledTime,

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
    eventInformation_eventDescription,
    eventInformation_eventSubType,

    -- * Explanation
    Explanation (..),
    newExplanation,
    explanation_loadBalancerTargetGroups,
    explanation_loadBalancerArn,
    explanation_securityGroupRule,
    explanation_customerGateway,
    explanation_availabilityZones,
    explanation_loadBalancerTargetGroup,
    explanation_explanationCode,
    explanation_address,
    explanation_natGateway,
    explanation_loadBalancerTarget,
    explanation_internetGateway,
    explanation_networkInterface,
    explanation_subnet,
    explanation_sourceVpc,
    explanation_aclRule,
    explanation_protocols,
    explanation_direction,
    explanation_securityGroups,
    explanation_prefixList,
    explanation_packetField,
    explanation_cidrs,
    explanation_component,
    explanation_state,
    explanation_routeTable,
    explanation_destination,
    explanation_subnetRouteTable,
    explanation_attachedTo,
    explanation_securityGroup,
    explanation_vpcPeeringConnection,
    explanation_destinationVpc,
    explanation_elasticLoadBalancerListener,
    explanation_port,
    explanation_acl,
    explanation_vpcEndpoint,
    explanation_routeTableRoute,
    explanation_vpnConnection,
    explanation_missingComponent,
    explanation_addresses,
    explanation_classicLoadBalancerListener,
    explanation_ingressRouteTable,
    explanation_vpnGateway,
    explanation_portRanges,
    explanation_loadBalancerListenerPort,
    explanation_vpc,
    explanation_loadBalancerTargetPort,

    -- * ExportImageTask
    ExportImageTask (..),
    newExportImageTask,
    exportImageTask_statusMessage,
    exportImageTask_status,
    exportImageTask_imageId,
    exportImageTask_tags,
    exportImageTask_s3ExportLocation,
    exportImageTask_description,
    exportImageTask_exportImageTaskId,
    exportImageTask_progress,

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

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    newFailedQueuedPurchaseDeletion,
    failedQueuedPurchaseDeletion_reservedInstancesId,
    failedQueuedPurchaseDeletion_error,

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

    -- * FleetData
    FleetData (..),
    newFleetData,
    fleetData_launchTemplateConfigs,
    fleetData_fleetState,
    fleetData_onDemandOptions,
    fleetData_fleetId,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_instances,
    fleetData_validUntil,
    fleetData_activityStatus,
    fleetData_tags,
    fleetData_createTime,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_errors,
    fleetData_type,
    fleetData_spotOptions,
    fleetData_targetCapacitySpecification,
    fleetData_fulfilledCapacity,
    fleetData_clientToken,
    fleetData_terminateInstancesWithExpiration,

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
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_weightedCapacity,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    newFleetLaunchTemplateOverridesRequest,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_maxPrice,
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

    -- * FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    newFleetSpotCapacityRebalanceRequest,
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
    flowLog_maxAggregationInterval,
    flowLog_creationTime,
    flowLog_deliverLogsStatus,
    flowLog_flowLogId,
    flowLog_logDestination,
    flowLog_trafficType,
    flowLog_logFormat,
    flowLog_logGroupName,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsErrorMessage,
    flowLog_tags,
    flowLog_logDestinationType,
    flowLog_flowLogStatus,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    newFpgaDeviceInfo,
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_name,
    fpgaDeviceInfo_count,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    newFpgaDeviceMemoryInfo,
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- * FpgaImage
    FpgaImage (..),
    newFpgaImage,
    fpgaImage_ownerAlias,
    fpgaImage_shellVersion,
    fpgaImage_ownerId,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_productCodes,
    fpgaImage_state,
    fpgaImage_pciId,
    fpgaImage_name,
    fpgaImage_updateTime,
    fpgaImage_tags,
    fpgaImage_public,
    fpgaImage_createTime,
    fpgaImage_description,
    fpgaImage_fpgaImageId,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    newFpgaImageAttribute,
    fpgaImageAttribute_productCodes,
    fpgaImageAttribute_name,
    fpgaImageAttribute_loadPermissions,
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
    gpuDeviceInfo_manufacturer,
    gpuDeviceInfo_name,
    gpuDeviceInfo_count,

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
    historyRecord_timestamp,
    historyRecord_eventInformation,

    -- * HistoryRecordEntry
    HistoryRecordEntry (..),
    newHistoryRecordEntry,
    historyRecordEntry_eventType,
    historyRecordEntry_timestamp,
    historyRecordEntry_eventInformation,

    -- * Host
    Host (..),
    newHost,
    host_ownerId,
    host_hostProperties,
    host_availableCapacity,
    host_hostReservationId,
    host_allocationTime,
    host_instances,
    host_availabilityZoneId,
    host_state,
    host_availabilityZone,
    host_releaseTime,
    host_tags,
    host_autoPlacement,
    host_hostRecovery,
    host_memberOfServiceLinkedResourceGroup,
    host_allowsMultipleInstanceTypes,
    host_hostId,
    host_clientToken,

    -- * HostInstance
    HostInstance (..),
    newHostInstance,
    hostInstance_ownerId,
    hostInstance_instanceId,
    hostInstance_instanceType,

    -- * HostOffering
    HostOffering (..),
    newHostOffering,
    hostOffering_instanceFamily,
    hostOffering_upfrontPrice,
    hostOffering_paymentOption,
    hostOffering_duration,
    hostOffering_offeringId,
    hostOffering_currencyCode,
    hostOffering_hourlyPrice,

    -- * HostProperties
    HostProperties (..),
    newHostProperties,
    hostProperties_instanceFamily,
    hostProperties_totalVCpus,
    hostProperties_instanceType,
    hostProperties_cores,
    hostProperties_sockets,

    -- * HostReservation
    HostReservation (..),
    newHostReservation,
    hostReservation_instanceFamily,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_upfrontPrice,
    hostReservation_paymentOption,
    hostReservation_duration,
    hostReservation_offeringId,
    hostReservation_hostReservationId,
    hostReservation_currencyCode,
    hostReservation_state,
    hostReservation_tags,
    hostReservation_count,
    hostReservation_hourlyPrice,
    hostReservation_start,

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
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_associationId,
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
    idFormat_useLongIds,
    idFormat_resource,
    idFormat_deadline,

    -- * Image
    Image (..),
    newImage,
    image_platform,
    image_rootDeviceName,
    image_ramdiskId,
    image_stateReason,
    image_usageOperation,
    image_creationDate,
    image_productCodes,
    image_platformDetails,
    image_name,
    image_tags,
    image_sriovNetSupport,
    image_blockDeviceMappings,
    image_kernelId,
    image_description,
    image_imageOwnerAlias,
    image_enaSupport,
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
    imageDiskContainer_description,
    imageDiskContainer_url,

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
    importImageTask_hypervisor,
    importImageTask_platform,
    importImageTask_statusMessage,
    importImageTask_status,
    importImageTask_snapshotDetails,
    importImageTask_encrypted,
    importImageTask_importTaskId,
    importImageTask_licenseSpecifications,
    importImageTask_architecture,
    importImageTask_imageId,
    importImageTask_kmsKeyId,
    importImageTask_tags,
    importImageTask_description,
    importImageTask_licenseType,
    importImageTask_progress,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    newImportInstanceLaunchSpecification,
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_privateIpAddress,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    newImportInstanceTaskDetails,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_description,

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    newImportInstanceVolumeDetailItem,
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_description,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    newImportSnapshotTask,
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_importTaskId,
    importSnapshotTask_tags,
    importSnapshotTask_description,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    newImportVolumeTaskDetails,
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_volume,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_description,

    -- * InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    newInferenceAcceleratorInfo,
    inferenceAcceleratorInfo_accelerators,

    -- * InferenceDeviceInfo
    InferenceDeviceInfo (..),
    newInferenceDeviceInfo,
    inferenceDeviceInfo_manufacturer,
    inferenceDeviceInfo_name,
    inferenceDeviceInfo_count,

    -- * Instance
    Instance (..),
    newInstance,
    instance_platform,
    instance_instanceLifecycle,
    instance_stateTransitionReason,
    instance_rootDeviceName,
    instance_capacityReservationSpecification,
    instance_ebsOptimized,
    instance_ramdiskId,
    instance_elasticInferenceAcceleratorAssociations,
    instance_stateReason,
    instance_outpostArn,
    instance_sourceDestCheck,
    instance_productCodes,
    instance_securityGroups,
    instance_iamInstanceProfile,
    instance_publicDnsName,
    instance_hibernationOptions,
    instance_capacityReservationId,
    instance_tags,
    instance_sriovNetSupport,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_subnetId,
    instance_enclaveOptions,
    instance_kernelId,
    instance_cpuOptions,
    instance_privateDnsName,
    instance_keyName,
    instance_networkInterfaces,
    instance_licenses,
    instance_vpcId,
    instance_elasticGpuAssociations,
    instance_metadataOptions,
    instance_enaSupport,
    instance_spotInstanceRequestId,
    instance_clientToken,
    instance_privateIpAddress,
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
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,
    instanceBlockDeviceMappingSpecification_deviceName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    newInstanceCapacity,
    instanceCapacity_instanceType,
    instanceCapacity_availableCapacity,
    instanceCapacity_totalCapacity,

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

    -- * InstanceExportDetails
    InstanceExportDetails (..),
    newInstanceExportDetails,
    instanceExportDetails_instanceId,
    instanceExportDetails_targetEnvironment,

    -- * InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    newInstanceFamilyCreditSpecification,
    instanceFamilyCreditSpecification_instanceFamily,
    instanceFamilyCreditSpecification_cpuCredits,

    -- * InstanceIpv6Address
    InstanceIpv6Address (..),
    newInstanceIpv6Address,
    instanceIpv6Address_ipv6Address,

    -- * InstanceIpv6AddressRequest
    InstanceIpv6AddressRequest (..),
    newInstanceIpv6AddressRequest,
    instanceIpv6AddressRequest_ipv6Address,

    -- * InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    newInstanceMarketOptionsRequest,
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- * InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    newInstanceMetadataOptionsRequest,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    newInstanceMetadataOptionsResponse,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpTokens,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    newInstanceMonitoring,
    instanceMonitoring_instanceId,
    instanceMonitoring_monitoring,

    -- * InstanceNetworkInterface
    InstanceNetworkInterface (..),
    newInstanceNetworkInterface,
    instanceNetworkInterface_groups,
    instanceNetworkInterface_status,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_association,
    instanceNetworkInterface_ipv6Addresses,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_description,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_privateIpAddress,

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    newInstanceNetworkInterfaceAssociation,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_publicIp,

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    newInstanceNetworkInterfaceAttachment,
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    newInstanceNetworkInterfaceSpecification,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_privateIpAddress,

    -- * InstancePrivateIpAddress
    InstancePrivateIpAddress (..),
    newInstancePrivateIpAddress,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_privateIpAddress,

    -- * InstanceSpecification
    InstanceSpecification (..),
    newInstanceSpecification,
    instanceSpecification_instanceId,
    instanceSpecification_excludeBootVolume,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_name,
    instanceState_code,

    -- * InstanceStateChange
    InstanceStateChange (..),
    newInstanceStateChange,
    instanceStateChange_instanceId,
    instanceStateChange_currentState,
    instanceStateChange_previousState,

    -- * InstanceStatus
    InstanceStatus (..),
    newInstanceStatus,
    instanceStatus_instanceId,
    instanceStatus_systemStatus,
    instanceStatus_outpostArn,
    instanceStatus_instanceStatus,
    instanceStatus_events,
    instanceStatus_availabilityZone,
    instanceStatus_instanceState,

    -- * InstanceStatusDetails
    InstanceStatusDetails (..),
    newInstanceStatusDetails,
    instanceStatusDetails_status,
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,

    -- * InstanceStatusEvent
    InstanceStatusEvent (..),
    newInstanceStatusEvent,
    instanceStatusEvent_notBefore,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_code,
    instanceStatusEvent_notAfter,
    instanceStatusEvent_notBeforeDeadline,
    instanceStatusEvent_description,

    -- * InstanceStatusSummary
    InstanceStatusSummary (..),
    newInstanceStatusSummary,
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- * InstanceStorageInfo
    InstanceStorageInfo (..),
    newInstanceStorageInfo,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_totalSizeInGB,
    instanceStorageInfo_disks,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    newInstanceTagNotificationAttribute,
    instanceTagNotificationAttribute_instanceTagKeys,
    instanceTagNotificationAttribute_includeAllTagsOfInstance,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    newInstanceTypeInfo,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_supportedVirtualizationTypes,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    newInstanceTypeOffering,
    instanceTypeOffering_instanceType,
    instanceTypeOffering_locationType,
    instanceTypeOffering_location,

    -- * InstanceUsage
    InstanceUsage (..),
    newInstanceUsage,
    instanceUsage_accountId,
    instanceUsage_usedInstanceCount,

    -- * InternetGateway
    InternetGateway (..),
    newInternetGateway,
    internetGateway_ownerId,
    internetGateway_tags,
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
    ipPermission_fromPort,
    ipPermission_prefixListIds,
    ipPermission_ipRanges,
    ipPermission_ipv6Ranges,
    ipPermission_userIdGroupPairs,
    ipPermission_toPort,
    ipPermission_ipProtocol,

    -- * IpRange
    IpRange (..),
    newIpRange,
    ipRange_description,
    ipRange_cidrIp,

    -- * Ipv6CidrAssociation
    Ipv6CidrAssociation (..),
    newIpv6CidrAssociation,
    ipv6CidrAssociation_ipv6Cidr,
    ipv6CidrAssociation_associatedResource,

    -- * Ipv6CidrBlock
    Ipv6CidrBlock (..),
    newIpv6CidrBlock,
    ipv6CidrBlock_ipv6CidrBlock,

    -- * Ipv6Pool
    Ipv6Pool (..),
    newIpv6Pool,
    ipv6Pool_poolId,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_tags,
    ipv6Pool_description,

    -- * Ipv6Range
    Ipv6Range (..),
    newIpv6Range,
    ipv6Range_cidrIpv6,
    ipv6Range_description,

    -- * KeyPairInfo
    KeyPairInfo (..),
    newKeyPairInfo,
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyPairId,
    keyPairInfo_tags,
    keyPairInfo_keyName,

    -- * LastError
    LastError (..),
    newLastError,
    lastError_message,
    lastError_code,

    -- * LaunchPermission
    LaunchPermission (..),
    newLaunchPermission,
    launchPermission_group,
    launchPermission_userId,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    newLaunchPermissionModifications,
    launchPermissionModifications_add,
    launchPermissionModifications_remove,

    -- * LaunchSpecification
    LaunchSpecification (..),
    newLaunchSpecification,
    launchSpecification_instanceType,
    launchSpecification_ebsOptimized,
    launchSpecification_userData,
    launchSpecification_placement,
    launchSpecification_addressingType,
    launchSpecification_ramdiskId,
    launchSpecification_imageId,
    launchSpecification_securityGroups,
    launchSpecification_iamInstanceProfile,
    launchSpecification_monitoring,
    launchSpecification_blockDeviceMappings,
    launchSpecification_subnetId,
    launchSpecification_kernelId,
    launchSpecification_keyName,
    launchSpecification_networkInterfaces,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_launchTemplateId,
    launchTemplate_launchTemplateName,
    launchTemplate_tags,
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_latestVersionNumber,

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    newLaunchTemplateAndOverridesResponse,
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    newLaunchTemplateBlockDeviceMapping,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,
    launchTemplateBlockDeviceMapping_deviceName,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    newLaunchTemplateBlockDeviceMappingRequest,
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_virtualName,
    launchTemplateBlockDeviceMappingRequest_deviceName,

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
    launchTemplateCpuOptions_threadsPerCore,
    launchTemplateCpuOptions_coreCount,

    -- * LaunchTemplateCpuOptionsRequest
    LaunchTemplateCpuOptionsRequest (..),
    newLaunchTemplateCpuOptionsRequest,
    launchTemplateCpuOptionsRequest_threadsPerCore,
    launchTemplateCpuOptionsRequest_coreCount,

    -- * LaunchTemplateEbsBlockDevice
    LaunchTemplateEbsBlockDevice (..),
    newLaunchTemplateEbsBlockDevice,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_iops,

    -- * LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (..),
    newLaunchTemplateEbsBlockDeviceRequest,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_iops,

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
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpTokens,

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    newLaunchTemplateInstanceMetadataOptionsRequest,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecification,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,

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
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_weightedCapacity,

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    newLaunchTemplatePlacement,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_hostId,

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    newLaunchTemplatePlacementRequest,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_hostId,

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
    launchTemplateSpotMarketOptions_validUntil,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_maxPrice,

    -- * LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    newLaunchTemplateSpotMarketOptionsRequest,
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_validUntil,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_maxPrice,

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
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,

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
    localGateway_ownerId,
    localGateway_outpostArn,
    localGateway_localGatewayId,
    localGateway_state,
    localGateway_tags,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    newLocalGatewayRoute,
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_state,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_type,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    newLocalGatewayRouteTable,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_state,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_tags,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- * LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (..),
    newLocalGatewayRouteTableVpcAssociation,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_vpcId,

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    newLocalGatewayVirtualInterface,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_vlan,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    newLocalGatewayVirtualInterfaceGroup,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_tags,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    newManagedPrefixList,
    managedPrefixList_stateMessage,
    managedPrefixList_ownerId,
    managedPrefixList_maxEntries,
    managedPrefixList_prefixListName,
    managedPrefixList_version,
    managedPrefixList_prefixListId,
    managedPrefixList_prefixListArn,
    managedPrefixList_state,
    managedPrefixList_tags,
    managedPrefixList_addressFamily,

    -- * MemoryInfo
    MemoryInfo (..),
    newMemoryInfo,
    memoryInfo_sizeInMiB,

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    newModifyTransitGatewayOptions,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,

    -- * ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (..),
    newModifyTransitGatewayVpcAttachmentRequestOptions,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- * ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (..),
    newModifyVpnTunnelOptionsSpecification,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,

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
    natGateway_natGatewayAddresses,
    natGateway_failureMessage,
    natGateway_failureCode,
    natGateway_provisionedBandwidth,
    natGateway_state,
    natGateway_deleteTime,
    natGateway_tags,
    natGateway_createTime,
    natGateway_subnetId,
    natGateway_natGatewayId,
    natGateway_vpcId,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    newNatGatewayAddress,
    natGatewayAddress_privateIp,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_publicIp,
    natGatewayAddress_allocationId,

    -- * NetworkAcl
    NetworkAcl (..),
    newNetworkAcl,
    networkAcl_ownerId,
    networkAcl_isDefault,
    networkAcl_tags,
    networkAcl_vpcId,
    networkAcl_networkAclId,
    networkAcl_associations,
    networkAcl_entries,

    -- * NetworkAclAssociation
    NetworkAclAssociation (..),
    newNetworkAclAssociation,
    networkAclAssociation_networkAclAssociationId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclId,

    -- * NetworkAclEntry
    NetworkAclEntry (..),
    newNetworkAclEntry,
    networkAclEntry_portRange,
    networkAclEntry_ruleAction,
    networkAclEntry_egress,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_protocol,
    networkAclEntry_ruleNumber,
    networkAclEntry_cidrBlock,

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
    networkInfo_efaSupported,
    networkInfo_networkCards,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_ipv6Supported,
    networkInfo_maximumNetworkCards,
    networkInfo_networkPerformance,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_enaSupport,

    -- * NetworkInsightsAnalysis
    NetworkInsightsAnalysis (..),
    newNetworkInsightsAnalysis,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_forwardPathComponents,

    -- * NetworkInsightsPath
    NetworkInsightsPath (..),
    newNetworkInsightsPath,
    networkInsightsPath_createdDate,
    networkInsightsPath_source,
    networkInsightsPath_destinationIp,
    networkInsightsPath_destination,
    networkInsightsPath_sourceIp,
    networkInsightsPath_tags,
    networkInsightsPath_protocol,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_destinationPort,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_groups,
    networkInterface_status,
    networkInterface_ownerId,
    networkInterface_privateIpAddresses,
    networkInterface_attachment,
    networkInterface_macAddress,
    networkInterface_association,
    networkInterface_ipv6Addresses,
    networkInterface_requesterManaged,
    networkInterface_requesterId,
    networkInterface_outpostArn,
    networkInterface_tagSet,
    networkInterface_interfaceType,
    networkInterface_sourceDestCheck,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_description,
    networkInterface_privateDnsName,
    networkInterface_vpcId,
    networkInterface_privateIpAddress,

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    newNetworkInterfaceAssociation,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_publicIp,
    networkInterfaceAssociation_allocationId,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    newNetworkInterfaceAttachment,
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_instanceOwnerId,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    newNetworkInterfaceAttachmentChanges,
    networkInterfaceAttachmentChanges_attachmentId,
    networkInterfaceAttachmentChanges_deleteOnTermination,

    -- * NetworkInterfaceIpv6Address
    NetworkInterfaceIpv6Address (..),
    newNetworkInterfaceIpv6Address,
    networkInterfaceIpv6Address_ipv6Address,

    -- * NetworkInterfacePermission
    NetworkInterfacePermission (..),
    newNetworkInterfacePermission,
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_permissionState,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_permission,
    networkInterfacePermission_awsService,
    networkInterfacePermission_networkInterfacePermissionId,

    -- * NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    newNetworkInterfacePermissionState,
    networkInterfacePermissionState_statusMessage,
    networkInterfacePermissionState_state,

    -- * NetworkInterfacePrivateIpAddress
    NetworkInterfacePrivateIpAddress (..),
    newNetworkInterfacePrivateIpAddress,
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_association,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_privateIpAddress,

    -- * NewDhcpConfiguration
    NewDhcpConfiguration (..),
    newNewDhcpConfiguration,
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- * OnDemandOptions
    OnDemandOptions (..),
    newOnDemandOptions,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_singleInstanceType,
    onDemandOptions_allocationStrategy,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_singleAvailabilityZone,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    newOnDemandOptionsRequest,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_singleAvailabilityZone,

    -- * PathComponent
    PathComponent (..),
    newPathComponent,
    pathComponent_securityGroupRule,
    pathComponent_sequenceNumber,
    pathComponent_subnet,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_inboundHeader,
    pathComponent_component,
    pathComponent_destinationVpc,
    pathComponent_routeTableRoute,
    pathComponent_outboundHeader,
    pathComponent_vpc,

    -- * PciId
    PciId (..),
    newPciId,
    pciId_subsystemId,
    pciId_subsystemVendorId,
    pciId_deviceId,
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
    placement_spreadDomain,
    placement_groupName,
    placement_tenancy,
    placement_affinity,
    placement_availabilityZone,
    placement_partitionNumber,
    placement_hostResourceGroupArn,
    placement_hostId,

    -- * PlacementGroup
    PlacementGroup (..),
    newPlacementGroup,
    placementGroup_strategy,
    placementGroup_groupName,
    placementGroup_groupId,
    placementGroup_state,
    placementGroup_tags,
    placementGroup_partitionCount,

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
    portRange_to,
    portRange_from,

    -- * PrefixList
    PrefixList (..),
    newPrefixList,
    prefixList_prefixListName,
    prefixList_cidrs,
    prefixList_prefixListId,

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
    priceSchedule_currencyCode,
    priceSchedule_active,
    priceSchedule_term,
    priceSchedule_price,

    -- * PriceScheduleSpecification
    PriceScheduleSpecification (..),
    newPriceScheduleSpecification,
    priceScheduleSpecification_currencyCode,
    priceScheduleSpecification_term,
    priceScheduleSpecification_price,

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
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_value,
    privateDnsNameConfiguration_type,

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
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_status,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_requested,
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
    publicIpv4Pool_poolId,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_tags,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_description,
    publicIpv4Pool_networkBorderGroup,

    -- * PublicIpv4PoolRange
    PublicIpv4PoolRange (..),
    newPublicIpv4PoolRange,
    publicIpv4PoolRange_addressCount,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_availableAddressCount,

    -- * Purchase
    Purchase (..),
    newPurchase,
    purchase_instanceFamily,
    purchase_hostIdSet,
    purchase_upfrontPrice,
    purchase_paymentOption,
    purchase_duration,
    purchase_hostReservationId,
    purchase_currencyCode,
    purchase_hourlyPrice,

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

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_regionName,
    regionInfo_optInStatus,
    regionInfo_endpoint,

    -- * RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    newRegisterInstanceTagAttributeRequest,
    registerInstanceTagAttributeRequest_instanceTagKeys,
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,

    -- * RemovePrefixListEntry
    RemovePrefixListEntry (..),
    newRemovePrefixListEntry,
    removePrefixListEntry_cidr,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    newRequestLaunchTemplateData,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_metadataOptions,

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    newRequestSpotLaunchSpecification,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_networkInterfaces,

    -- * Reservation
    Reservation (..),
    newReservation,
    reservation_groups,
    reservation_requesterId,
    reservation_instances,
    reservation_reservationId,
    reservation_ownerId,

    -- * ReservationValue
    ReservationValue (..),
    newReservationValue,
    reservationValue_remainingUpfrontValue,
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,

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
    reservedInstances_end,
    reservedInstances_instanceType,
    reservedInstances_duration,
    reservedInstances_reservedInstancesId,
    reservedInstances_instanceTenancy,
    reservedInstances_currencyCode,
    reservedInstances_scope,
    reservedInstances_state,
    reservedInstances_availabilityZone,
    reservedInstances_tags,
    reservedInstances_offeringClass,
    reservedInstances_fixedPrice,
    reservedInstances_usagePrice,
    reservedInstances_offeringType,
    reservedInstances_start,
    reservedInstances_recurringCharges,
    reservedInstances_productDescription,
    reservedInstances_instanceCount,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    newReservedInstancesConfiguration,
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_scope,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceCount,

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    newReservedInstancesId,
    reservedInstancesId_reservedInstancesId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    newReservedInstancesListing,
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_status,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_createDate,
    reservedInstancesListing_tags,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_clientToken,

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    newReservedInstancesModification,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_status,
    reservedInstancesModification_createDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_clientToken,

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    newReservedInstancesModificationResult,
    reservedInstancesModificationResult_targetConfiguration,
    reservedInstancesModificationResult_reservedInstancesId,

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    newReservedInstancesOffering,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_usagePrice,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_pricingDetails,

    -- * ResponseError
    ResponseError (..),
    newResponseError,
    responseError_message,
    responseError_code,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    newResponseLaunchTemplateData,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_metadataOptions,

    -- * Route
    Route (..),
    newRoute,
    route_instanceId,
    route_origin,
    route_vpcPeeringConnectionId,
    route_destinationPrefixListId,
    route_destinationIpv6CidrBlock,
    route_localGatewayId,
    route_state,
    route_egressOnlyInternetGatewayId,
    route_carrierGatewayId,
    route_destinationCidrBlock,
    route_networkInterfaceId,
    route_natGatewayId,
    route_instanceOwnerId,
    route_gatewayId,
    route_transitGatewayId,

    -- * RouteTable
    RouteTable (..),
    newRouteTable,
    routeTable_ownerId,
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_tags,
    routeTable_propagatingVgws,
    routeTable_vpcId,
    routeTable_associations,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    newRouteTableAssociation,
    routeTableAssociation_associationState,
    routeTableAssociation_main,
    routeTableAssociation_routeTableId,
    routeTableAssociation_subnetId,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_gatewayId,

    -- * RouteTableAssociationState
    RouteTableAssociationState (..),
    newRouteTableAssociationState,
    routeTableAssociationState_statusMessage,
    routeTableAssociationState_state,

    -- * RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (..),
    newRunInstancesMonitoringEnabled,
    runInstancesMonitoringEnabled_enabled,

    -- * S3Storage
    S3Storage (..),
    newS3Storage,
    s3Storage_uploadPolicySignature,
    s3Storage_uploadPolicy,
    s3Storage_prefix,
    s3Storage_bucket,
    s3Storage_aWSAccessKeyId,

    -- * ScheduledInstance
    ScheduledInstance (..),
    newScheduledInstance,
    scheduledInstance_platform,
    scheduledInstance_instanceType,
    scheduledInstance_networkPlatform,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_createDate,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_availabilityZone,
    scheduledInstance_recurrence,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_hourlyPrice,
    scheduledInstance_termEndDate,
    scheduledInstance_termStartDate,
    scheduledInstance_instanceCount,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    newScheduledInstanceAvailability,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_purchaseToken,

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    newScheduledInstanceRecurrence,
    scheduledInstanceRecurrence_occurrenceUnit,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceDaySet,

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    newScheduledInstanceRecurrenceRequest,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_frequency,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    newScheduledInstancesBlockDeviceMapping,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,
    scheduledInstancesBlockDeviceMapping_deviceName,

    -- * ScheduledInstancesEbs
    ScheduledInstancesEbs (..),
    newScheduledInstancesEbs,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_iops,

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
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_imageId,

    -- * ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    newScheduledInstancesMonitoring,
    scheduledInstancesMonitoring_enabled,

    -- * ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    newScheduledInstancesNetworkInterface,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_privateIpAddress,

    -- * ScheduledInstancesPlacement
    ScheduledInstancesPlacement (..),
    newScheduledInstancesPlacement,
    scheduledInstancesPlacement_groupName,
    scheduledInstancesPlacement_availabilityZone,

    -- * ScheduledInstancesPrivateIpAddressConfig
    ScheduledInstancesPrivateIpAddressConfig (..),
    newScheduledInstancesPrivateIpAddressConfig,
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- * SecurityGroup
    SecurityGroup (..),
    newSecurityGroup,
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
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
    securityGroupReference_groupId,
    securityGroupReference_referencingVpcId,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    newServiceConfiguration,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_serviceId,
    serviceConfiguration_serviceName,
    serviceConfiguration_tags,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_serviceType,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_serviceState,

    -- * ServiceDetail
    ServiceDetail (..),
    newServiceDetail,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_availabilityZones,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_serviceId,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_privateDnsNames,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_serviceName,
    serviceDetail_tags,
    serviceDetail_owner,
    serviceDetail_privateDnsName,
    serviceDetail_acceptanceRequired,
    serviceDetail_serviceType,

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
    snapshot_ownerAlias,
    snapshot_stateMessage,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
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
    snapshotDetail_diskImageSize,
    snapshotDetail_statusMessage,
    snapshotDetail_status,
    snapshotDetail_format,
    snapshotDetail_userBucket,
    snapshotDetail_deviceName,
    snapshotDetail_snapshotId,
    snapshotDetail_description,
    snapshotDetail_url,
    snapshotDetail_progress,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    newSnapshotDiskContainer,
    snapshotDiskContainer_format,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_description,
    snapshotDiskContainer_url,

    -- * SnapshotInfo
    SnapshotInfo (..),
    newSnapshotInfo,
    snapshotInfo_ownerId,
    snapshotInfo_encrypted,
    snapshotInfo_outpostArn,
    snapshotInfo_startTime,
    snapshotInfo_volumeId,
    snapshotInfo_state,
    snapshotInfo_snapshotId,
    snapshotInfo_tags,
    snapshotInfo_description,
    snapshotInfo_progress,
    snapshotInfo_volumeSize,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    newSnapshotTaskDetail,
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_status,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_format,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_description,
    snapshotTaskDetail_url,
    snapshotTaskDetail_progress,

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    newSpotCapacityRebalance,
    spotCapacityRebalance_replacementStrategy,

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    newSpotDatafeedSubscription,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_bucket,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    newSpotFleetLaunchSpecification,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_weightedCapacity,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    newSpotFleetMonitoring,
    spotFleetMonitoring_enabled,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    newSpotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    newSpotFleetRequestConfigData,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
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
    spotInstanceRequest_status,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_fault,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_state,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_tags,
    spotInstanceRequest_createTime,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_type,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotInstanceRequestId,

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
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_validUntil,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_maxPrice,

    -- * SpotOptions
    SpotOptions (..),
    newSpotOptions,
    spotOptions_minTargetCapacity,
    spotOptions_maintenanceStrategies,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_singleInstanceType,
    spotOptions_allocationStrategy,
    spotOptions_maxTotalPrice,
    spotOptions_instancePoolsToUseCount,
    spotOptions_singleAvailabilityZone,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    newSpotOptionsRequest,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_singleAvailabilityZone,

    -- * SpotPlacement
    SpotPlacement (..),
    newSpotPlacement,
    spotPlacement_groupName,
    spotPlacement_tenancy,
    spotPlacement_availabilityZone,

    -- * SpotPrice
    SpotPrice (..),
    newSpotPrice,
    spotPrice_instanceType,
    spotPrice_spotPrice,
    spotPrice_availabilityZone,
    spotPrice_timestamp,
    spotPrice_productDescription,

    -- * StaleIpPermission
    StaleIpPermission (..),
    newStaleIpPermission,
    staleIpPermission_fromPort,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipRanges,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_ipProtocol,
    staleIpPermission_toPort,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    newStaleSecurityGroup,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_groupName,
    staleSecurityGroup_groupId,
    staleSecurityGroup_description,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,

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

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_ownerId,
    subnet_customerOwnedIpv4Pool,
    subnet_subnetArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_outpostArn,
    subnet_mapPublicIpOnLaunch,
    subnet_availabilityZoneId,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_tags,
    subnet_defaultForAz,
    subnet_mapCustomerOwnedIpOnLaunch,
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
    subnetCidrBlockState_statusMessage,
    subnetCidrBlockState_state,

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
    tagSpecification_resourceType,
    tagSpecification_tags,

    -- * TargetCapacitySpecification
    TargetCapacitySpecification (..),
    newTargetCapacitySpecification,
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    newTargetCapacitySpecificationRequest,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_spotTargetCapacity,
    targetCapacitySpecificationRequest_totalTargetCapacity,

    -- * TargetConfiguration
    TargetConfiguration (..),
    newTargetConfiguration,
    targetConfiguration_offeringId,
    targetConfiguration_instanceCount,

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
    targetNetwork_clientVpnEndpointId,
    targetNetwork_status,
    targetNetwork_securityGroups,
    targetNetwork_associationId,
    targetNetwork_vpcId,
    targetNetwork_targetNetworkId,

    -- * TargetReservationValue
    TargetReservationValue (..),
    newTargetReservationValue,
    targetReservationValue_targetConfiguration,
    targetReservationValue_reservationValue,

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    newTerminateConnectionStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_previousStatus,
    terminateConnectionStatus_currentStatus,

    -- * TrafficMirrorFilter
    TrafficMirrorFilter (..),
    newTrafficMirrorFilter,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_tags,
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_description,
    trafficMirrorFilter_ingressFilterRules,

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    newTrafficMirrorFilterRule,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_destinationPortRange,

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
    trafficMirrorSession_ownerId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_description,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_virtualNetworkId,

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    newTrafficMirrorTarget,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_tags,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_description,
    trafficMirrorTarget_type,
    trafficMirrorTarget_trafficMirrorTargetId,

    -- * TransitGateway
    TransitGateway (..),
    newTransitGateway,
    transitGateway_ownerId,
    transitGateway_creationTime,
    transitGateway_options,
    transitGateway_transitGatewayArn,
    transitGateway_state,
    transitGateway_tags,
    transitGateway_description,
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
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_association,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_state,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_transitGatewayOwnerId,
    transitGatewayAttachment_transitGatewayId,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    newTransitGatewayAttachmentAssociation,
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- * TransitGatewayAttachmentBgpConfiguration
    TransitGatewayAttachmentBgpConfiguration (..),
    newTransitGatewayAttachmentBgpConfiguration,
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
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
    transitGatewayConnect_transportTransitGatewayAttachmentId,
    transitGatewayConnect_transitGatewayId,

    -- * TransitGatewayConnectOptions
    TransitGatewayConnectOptions (..),
    newTransitGatewayConnectOptions,
    transitGatewayConnectOptions_protocol,

    -- * TransitGatewayConnectPeer
    TransitGatewayConnectPeer (..),
    newTransitGatewayConnectPeer,
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayAttachmentId,

    -- * TransitGatewayConnectPeerConfiguration
    TransitGatewayConnectPeerConfiguration (..),
    newTransitGatewayConnectPeerConfiguration,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,

    -- * TransitGatewayConnectRequestBgpOptions
    TransitGatewayConnectRequestBgpOptions (..),
    newTransitGatewayConnectRequestBgpOptions,
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- * TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    newTransitGatewayMulticastDeregisteredGroupMembers,
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,

    -- * TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    newTransitGatewayMulticastDeregisteredGroupSources,
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,

    -- * TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    newTransitGatewayMulticastDomain,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_transitGatewayId,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    newTransitGatewayMulticastDomainAssociation,
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    newTransitGatewayMulticastDomainAssociations,
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_subnets,

    -- * TransitGatewayMulticastDomainOptions
    TransitGatewayMulticastDomainOptions (..),
    newTransitGatewayMulticastDomainOptions,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    newTransitGatewayMulticastGroup,
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_sourceType,

    -- * TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    newTransitGatewayMulticastRegisteredGroupMembers,
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,

    -- * TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    newTransitGatewayMulticastRegisteredGroupSources,
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,

    -- * TransitGatewayOptions
    TransitGatewayOptions (..),
    newTransitGatewayOptions,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_transitGatewayCidrBlocks,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_defaultRouteTablePropagation,

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    newTransitGatewayPeeringAttachment,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,

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
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,

    -- * TransitGatewayPropagation
    TransitGatewayPropagation (..),
    newTransitGatewayPropagation,
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_state,
    transitGatewayPropagation_transitGatewayAttachmentId,
    transitGatewayPropagation_transitGatewayRouteTableId,

    -- * TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    newTransitGatewayRequestOptions,
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_defaultRouteTablePropagation,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    newTransitGatewayRoute,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_type,
    transitGatewayRoute_transitGatewayAttachments,

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
    transitGatewayRouteTable_transitGatewayRouteTableId,
    transitGatewayRouteTable_transitGatewayId,

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

    -- * TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (..),
    newTransitGatewayVpcAttachment,
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_vpcOwnerId,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_transitGatewayId,

    -- * TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (..),
    newTransitGatewayVpcAttachmentOptions,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,

    -- * TunnelOption
    TunnelOption (..),
    newTunnelOption,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_ikeVersions,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_startupAction,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_replayWindowSize,
    tunnelOption_outsideIpAddress,
    tunnelOption_preSharedKey,
    tunnelOption_tunnelInsideIpv6Cidr,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_phase2LifetimeSeconds,

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
    userIdGroupPair_groupId,
    userIdGroupPair_userId,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_description,
    userIdGroupPair_vpcId,

    -- * VCpuInfo
    VCpuInfo (..),
    newVCpuInfo,
    vCpuInfo_defaultCores,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validThreadsPerCore,
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
    vgwTelemetry_statusMessage,
    vgwTelemetry_status,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_certificateArn,
    vgwTelemetry_outsideIpAddress,

    -- * Volume
    Volume (..),
    newVolume,
    volume_multiAttachEnabled,
    volume_fastRestored,
    volume_outpostArn,
    volume_throughput,
    volume_kmsKeyId,
    volume_tags,
    volume_iops,
    volume_attachments,
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
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_state,
    volumeAttachment_deleteOnTermination,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_size,

    -- * VolumeModification
    VolumeModification (..),
    newVolumeModification,
    volumeModification_statusMessage,
    volumeModification_targetVolumeType,
    volumeModification_originalVolumeType,
    volumeModification_startTime,
    volumeModification_volumeId,
    volumeModification_endTime,
    volumeModification_originalIops,
    volumeModification_targetSize,
    volumeModification_originalSize,
    volumeModification_targetIops,
    volumeModification_modificationState,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_targetThroughput,
    volumeModification_originalThroughput,
    volumeModification_progress,

    -- * VolumeStatusAction
    VolumeStatusAction (..),
    newVolumeStatusAction,
    volumeStatusAction_eventType,
    volumeStatusAction_eventId,
    volumeStatusAction_code,
    volumeStatusAction_description,

    -- * VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    newVolumeStatusAttachmentStatus,
    volumeStatusAttachmentStatus_instanceId,
    volumeStatusAttachmentStatus_ioPerformance,

    -- * VolumeStatusDetails
    VolumeStatusDetails (..),
    newVolumeStatusDetails,
    volumeStatusDetails_status,
    volumeStatusDetails_name,

    -- * VolumeStatusEvent
    VolumeStatusEvent (..),
    newVolumeStatusEvent,
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventType,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_eventId,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_description,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    newVolumeStatusInfo,
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    newVolumeStatusItem,
    volumeStatusItem_volumeStatus,
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeId,
    volumeStatusItem_actions,
    volumeStatusItem_events,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_attachmentStatuses,

    -- * Vpc
    Vpc (..),
    newVpc,
    vpc_ownerId,
    vpc_isDefault,
    vpc_cidrBlockAssociationSet,
    vpc_ipv6CidrBlockAssociationSet,
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
    vpcCidrBlockAssociation_cidrBlockState,
    vpcCidrBlockAssociation_associationId,
    vpcCidrBlockAssociation_cidrBlock,

    -- * VpcCidrBlockState
    VpcCidrBlockState (..),
    newVpcCidrBlockState,
    vpcCidrBlockState_statusMessage,
    vpcCidrBlockState_state,

    -- * VpcClassicLink
    VpcClassicLink (..),
    newVpcClassicLink,
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_vpcId,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_policyDocument,
    vpcEndpoint_groups,
    vpcEndpoint_ownerId,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_subnetIds,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_serviceName,
    vpcEndpoint_lastError,
    vpcEndpoint_state,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_tags,
    vpcEndpoint_vpcId,

    -- * VpcEndpointConnection
    VpcEndpointConnection (..),
    newVpcEndpointConnection,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_networkLoadBalancerArns,

    -- * VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (..),
    newVpcIpv6CidrBlockAssociation,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_status,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_tags,

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
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_vpcId,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_peeringOptions,

    -- * VpnConnection
    VpnConnection (..),
    newVpnConnection,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_options,
    vpnConnection_category,
    vpnConnection_routes,
    vpnConnection_tags,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnGatewayId,
    vpnConnection_transitGatewayId,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- * VpnConnectionOptions
    VpnConnectionOptions (..),
    newVpnConnectionOptions,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_localIpv4NetworkCidr,

    -- * VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (..),
    newVpnConnectionOptionsSpecification,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,

    -- * VpnGateway
    VpnGateway (..),
    newVpnGateway,
    vpnGateway_vpcAttachments,
    vpnGateway_state,
    vpnGateway_availabilityZone,
    vpnGateway_amazonSideAsn,
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_vpnGatewayId,

    -- * VpnStaticRoute
    VpnStaticRoute (..),
    newVpnStaticRoute,
    vpnStaticRoute_source,
    vpnStaticRoute_state,
    vpnStaticRoute_destinationCidrBlock,

    -- * VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (..),
    newVpnTunnelOptionsSpecification,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
  )
where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AccountAttribute
import Network.AWS.EC2.Types.AccountAttributeName
import Network.AWS.EC2.Types.AccountAttributeValue
import Network.AWS.EC2.Types.ActiveInstance
import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.AddPrefixListEntry
import Network.AWS.EC2.Types.Address
import Network.AWS.EC2.Types.AddressAttribute
import Network.AWS.EC2.Types.AddressAttributeName
import Network.AWS.EC2.Types.AddressStatus
import Network.AWS.EC2.Types.Affinity
import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllocationStrategy
import Network.AWS.EC2.Types.AllowedPrincipal
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.AlternatePathHint
import Network.AWS.EC2.Types.AnalysisAclRule
import Network.AWS.EC2.Types.AnalysisComponent
import Network.AWS.EC2.Types.AnalysisLoadBalancerListener
import Network.AWS.EC2.Types.AnalysisLoadBalancerTarget
import Network.AWS.EC2.Types.AnalysisPacketHeader
import Network.AWS.EC2.Types.AnalysisRouteTableRoute
import Network.AWS.EC2.Types.AnalysisSecurityGroupRule
import Network.AWS.EC2.Types.AnalysisStatus
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.ArchitectureType
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.AssignedPrivateIpAddress
import Network.AWS.EC2.Types.AssociatedNetworkType
import Network.AWS.EC2.Types.AssociatedRole
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.AssociationStatus
import Network.AWS.EC2.Types.AssociationStatusCode
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.EC2.Types.AttributeBooleanValue
import Network.AWS.EC2.Types.AttributeValue
import Network.AWS.EC2.Types.AuthorizationRule
import Network.AWS.EC2.Types.AutoAcceptSharedAssociationsValue
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailabilityZone
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.BgpStatus
import Network.AWS.EC2.Types.BlobAttributeValue
import Network.AWS.EC2.Types.BlockDeviceMapping
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
import Network.AWS.EC2.Types.ClassicLinkDnsSupport
import Network.AWS.EC2.Types.ClassicLinkInstance
import Network.AWS.EC2.Types.ClassicLoadBalancer
import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
import Network.AWS.EC2.Types.ClientConnectOptions
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientData
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
import Network.AWS.EC2.Types.ClientVpnEndpointStatus
import Network.AWS.EC2.Types.ClientVpnEndpointStatusCode
import Network.AWS.EC2.Types.ClientVpnRoute
import Network.AWS.EC2.Types.ClientVpnRouteStatus
import Network.AWS.EC2.Types.ClientVpnRouteStatusCode
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
import Network.AWS.EC2.Types.CpuOptions
import Network.AWS.EC2.Types.CpuOptionsRequest
import Network.AWS.EC2.Types.CreateFleetError
import Network.AWS.EC2.Types.CreateFleetInstance
import Network.AWS.EC2.Types.CreateTransitGatewayConnectRequestOptions
import Network.AWS.EC2.Types.CreateTransitGatewayMulticastDomainRequestOptions
import Network.AWS.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.CreateVolumePermission
import Network.AWS.EC2.Types.CreateVolumePermissionModifications
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.CustomerGateway
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
import Network.AWS.EC2.Types.DhcpConfiguration
import Network.AWS.EC2.Types.DhcpOptions
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
import Network.AWS.EC2.Types.Explanation
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
import Network.AWS.EC2.Types.HttpTokensState
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.IamInstanceProfile
import Network.AWS.EC2.Types.IamInstanceProfileAssociation
import Network.AWS.EC2.Types.IamInstanceProfileAssociationState
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.IcmpTypeCode
import Network.AWS.EC2.Types.IdFormat
import Network.AWS.EC2.Types.Igmpv2SupportValue
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
import Network.AWS.EC2.Types.InternetGateway
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.IpPermission
import Network.AWS.EC2.Types.IpRange
import Network.AWS.EC2.Types.Ipv6CidrAssociation
import Network.AWS.EC2.Types.Ipv6CidrBlock
import Network.AWS.EC2.Types.Ipv6Pool
import Network.AWS.EC2.Types.Ipv6Range
import Network.AWS.EC2.Types.Ipv6SupportValue
import Network.AWS.EC2.Types.KeyPairInfo
import Network.AWS.EC2.Types.LastError
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
import Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociation
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
import Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.ModifyVpnTunnelOptionsSpecification
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.EC2.Types.MonitoringState
import Network.AWS.EC2.Types.MoveStatus
import Network.AWS.EC2.Types.MovingAddressStatus
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.NatGateway
import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NatGatewayState
import Network.AWS.EC2.Types.NetworkAcl
import Network.AWS.EC2.Types.NetworkAclAssociation
import Network.AWS.EC2.Types.NetworkAclEntry
import Network.AWS.EC2.Types.NetworkCardInfo
import Network.AWS.EC2.Types.NetworkInfo
import Network.AWS.EC2.Types.NetworkInsightsAnalysis
import Network.AWS.EC2.Types.NetworkInsightsPath
import Network.AWS.EC2.Types.NetworkInterface
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
import Network.AWS.EC2.Types.NetworkInterfaceAttribute
import Network.AWS.EC2.Types.NetworkInterfaceCreationType
import Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
import Network.AWS.EC2.Types.NetworkInterfacePermission
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
import Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.NewDhcpConfiguration
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.OnDemandAllocationStrategy
import Network.AWS.EC2.Types.OnDemandOptions
import Network.AWS.EC2.Types.OnDemandOptionsRequest
import Network.AWS.EC2.Types.OperationType
import Network.AWS.EC2.Types.PathComponent
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
import Network.AWS.EC2.Types.PrivateDnsDetails
import Network.AWS.EC2.Types.PrivateDnsNameConfiguration
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import Network.AWS.EC2.Types.ProcessorInfo
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.ProductCodeValues
import Network.AWS.EC2.Types.PropagatingVgw
import Network.AWS.EC2.Types.Protocol
import Network.AWS.EC2.Types.ProtocolValue
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.PtrUpdateStatus
import Network.AWS.EC2.Types.PublicIpv4Pool
import Network.AWS.EC2.Types.PublicIpv4PoolRange
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
import Network.AWS.EC2.Types.StaleIpPermission
import Network.AWS.EC2.Types.StaleSecurityGroup
import Network.AWS.EC2.Types.State
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.StaticSourcesSupportValue
import Network.AWS.EC2.Types.StatusName
import Network.AWS.EC2.Types.StatusType
import Network.AWS.EC2.Types.Storage
import Network.AWS.EC2.Types.StorageLocation
import Network.AWS.EC2.Types.Subnet
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.SubnetCidrBlockState
import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
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
import Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayConnect
import Network.AWS.EC2.Types.TransitGatewayConnectOptions
import Network.AWS.EC2.Types.TransitGatewayConnectPeer
import Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
import Network.AWS.EC2.Types.TransitGatewayConnectPeerState
import Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions
import Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayMulticastDomain
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions
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
import Network.AWS.EC2.Types.TransitGatewayVpcAttachment
import Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.TunnelInsideIpVersion
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
import Network.AWS.EC2.Types.VCpuInfo
import Network.AWS.EC2.Types.ValidationError
import Network.AWS.EC2.Types.ValidationWarning
import Network.AWS.EC2.Types.VgwTelemetry
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
import Network.AWS.EC2.Types.Vpc
import Network.AWS.EC2.Types.VpcAttachment
import Network.AWS.EC2.Types.VpcAttributeName
import Network.AWS.EC2.Types.VpcCidrBlockAssociation
import Network.AWS.EC2.Types.VpcCidrBlockState
import Network.AWS.EC2.Types.VpcCidrBlockStateCode
import Network.AWS.EC2.Types.VpcClassicLink
import Network.AWS.EC2.Types.VpcEndpoint
import Network.AWS.EC2.Types.VpcEndpointConnection
import Network.AWS.EC2.Types.VpcEndpointType
import Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.VpcPeeringConnection
import Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
import Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
import Network.AWS.EC2.Types.VpcState
import Network.AWS.EC2.Types.VpcTenancy
import Network.AWS.EC2.Types.VpnConnection
import Network.AWS.EC2.Types.VpnConnectionOptions
import Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
import Network.AWS.EC2.Types.VpnEcmpSupportValue
import Network.AWS.EC2.Types.VpnGateway
import Network.AWS.EC2.Types.VpnProtocol
import Network.AWS.EC2.Types.VpnState
import Network.AWS.EC2.Types.VpnStaticRoute
import Network.AWS.EC2.Types.VpnStaticRouteSource
import Network.AWS.EC2.Types.VpnTunnelOptionsSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "EC2",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "ec2",
      Prelude._svcVersion = "2016-11-15",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "EC2",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has
          ( Prelude.hasCode "RequestLimitExceeded"
              Prelude.. Prelude.hasStatus 503
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "EC2ThrottledException"
              Prelude.. Prelude.hasStatus 503
          )
          e =
        Prelude.Just "ec2_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing
