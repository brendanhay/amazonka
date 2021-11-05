{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- * BootModeType
    BootModeType (..),

    -- * BootModeValues
    BootModeValues (..),

    -- * BundleTaskState
    BundleTaskState (..),

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

    -- * InstanceEventWindowState
    InstanceEventWindowState (..),

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

    -- * Ipv6SupportValue
    Ipv6SupportValue (..),

    -- * KeyType
    KeyType (..),

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

    -- * PartitionLoadFrequency
    PartitionLoadFrequency (..),

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

    -- * SubnetCidrReservationType
    SubnetCidrReservationType (..),

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

    -- * WeekDay
    WeekDay (..),

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
    activeInstance_instanceId,
    activeInstance_instanceHealth,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,

    -- * AddPrefixListEntry
    AddPrefixListEntry (..),
    newAddPrefixListEntry,
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- * Address
    Address (..),
    newAddress,
    address_associationId,
    address_instanceId,
    address_networkInterfaceOwnerId,
    address_allocationId,
    address_carrierIp,
    address_networkBorderGroup,
    address_domain,
    address_networkInterfaceId,
    address_publicIpv4Pool,
    address_customerOwnedIpv4Pool,
    address_customerOwnedIp,
    address_privateIpAddress,
    address_publicIp,
    address_tags,

    -- * AddressAttribute
    AddressAttribute (..),
    newAddressAttribute,
    addressAttribute_ptrRecordUpdate,
    addressAttribute_allocationId,
    addressAttribute_publicIp,
    addressAttribute_ptrRecord,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    newAllowedPrincipal,
    allowedPrincipal_principalType,
    allowedPrincipal_principal,

    -- * AlternatePathHint
    AlternatePathHint (..),
    newAlternatePathHint,
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- * AnalysisAclRule
    AnalysisAclRule (..),
    newAnalysisAclRule,
    analysisAclRule_ruleNumber,
    analysisAclRule_ruleAction,
    analysisAclRule_protocol,
    analysisAclRule_portRange,
    analysisAclRule_cidr,
    analysisAclRule_egress,

    -- * AnalysisComponent
    AnalysisComponent (..),
    newAnalysisComponent,
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
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_port,
    analysisLoadBalancerTarget_instance,

    -- * AnalysisPacketHeader
    AnalysisPacketHeader (..),
    newAnalysisPacketHeader,
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_protocol,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_sourcePortRanges,

    -- * AnalysisRouteTableRoute
    AnalysisRouteTableRoute (..),
    newAnalysisRouteTableRoute,
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_destinationPrefixListId,

    -- * AnalysisSecurityGroupRule
    AnalysisSecurityGroupRule (..),
    newAnalysisSecurityGroupRule,
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_prefixListId,

    -- * AssignedPrivateIpAddress
    AssignedPrivateIpAddress (..),
    newAssignedPrivateIpAddress,
    assignedPrivateIpAddress_privateIpAddress,

    -- * AssociatedRole
    AssociatedRole (..),
    newAssociatedRole,
    associatedRole_certificateS3BucketName,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_associatedRoleArn,

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
    athenaIntegration_partitionStartDate,
    athenaIntegration_partitionEndDate,
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
    authorizationRule_status,
    authorizationRule_accessAll,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_groupId,
    authorizationRule_destinationCidr,
    authorizationRule_description,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_state,
    availabilityZone_parentZoneId,
    availabilityZone_regionName,
    availabilityZone_parentZoneName,
    availabilityZone_networkBorderGroup,
    availabilityZone_zoneId,
    availabilityZone_zoneName,
    availabilityZone_optInStatus,
    availabilityZone_messages,
    availabilityZone_groupName,
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

    -- * BlobAttributeValue
    BlobAttributeValue (..),
    newBlobAttributeValue,
    blobAttributeValue_value,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
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
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_statusMessage,
    byoipCidr_description,

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
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,

    -- * CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    newCancelledSpotInstanceRequest,
    cancelledSpotInstanceRequest_state,
    cancelledSpotInstanceRequest_spotInstanceRequestId,

    -- * CapacityReservation
    CapacityReservation (..),
    newCapacityReservation,
    capacityReservation_capacityReservationFleetId,
    capacityReservation_state,
    capacityReservation_availabilityZoneId,
    capacityReservation_createDate,
    capacityReservation_outpostArn,
    capacityReservation_endDate,
    capacityReservation_availableInstanceCount,
    capacityReservation_ephemeralStorage,
    capacityReservation_instancePlatform,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_capacityReservationId,
    capacityReservation_instanceType,
    capacityReservation_ebsOptimized,
    capacityReservation_ownerId,
    capacityReservation_startDate,
    capacityReservation_availabilityZone,
    capacityReservation_tenancy,
    capacityReservation_totalInstanceCount,
    capacityReservation_endDateType,
    capacityReservation_tags,
    capacityReservation_capacityReservationArn,

    -- * CapacityReservationFleet
    CapacityReservationFleet (..),
    newCapacityReservationFleet,
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_state,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_endDate,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_totalTargetCapacity,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_tenancy,
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_createTime,
    capacityReservationFleet_tags,

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
    capacityReservationSpecification_capacityReservationTarget,
    capacityReservationSpecification_capacityReservationPreference,

    -- * CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (..),
    newCapacityReservationSpecificationResponse,
    capacityReservationSpecificationResponse_capacityReservationTarget,
    capacityReservationSpecificationResponse_capacityReservationPreference,

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
    carrierGateway_state,
    carrierGateway_vpcId,
    carrierGateway_ownerId,
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,

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
    classicLinkDnsSupport_vpcId,
    classicLinkDnsSupport_classicLinkDnsSupported,

    -- * ClassicLinkInstance
    ClassicLinkInstance (..),
    newClassicLinkInstance,
    classicLinkInstance_instanceId,
    classicLinkInstance_groups,
    classicLinkInstance_vpcId,
    classicLinkInstance_tags,

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
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,

    -- * ClientData
    ClientData (..),
    newClientData,
    clientData_uploadStart,
    clientData_uploadSize,
    clientData_uploadEnd,
    clientData_comment,

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
    clientVpnConnection_ingressPackets,
    clientVpnConnection_status,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_commonName,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_connectionId,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_username,
    clientVpnConnection_egressBytes,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_clientIp,
    clientVpnConnection_egressPackets,
    clientVpnConnection_timestamp,

    -- * ClientVpnConnectionStatus
    ClientVpnConnectionStatus (..),
    newClientVpnConnectionStatus,
    clientVpnConnectionStatus_code,
    clientVpnConnectionStatus_message,

    -- * ClientVpnEndpoint
    ClientVpnEndpoint (..),
    newClientVpnEndpoint,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_status,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_description,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnProtocol,
    clientVpnEndpoint_tags,

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
    clientVpnRoute_status,
    clientVpnRoute_origin,
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_type,
    clientVpnRoute_description,

    -- * ClientVpnRouteStatus
    ClientVpnRouteStatus (..),
    newClientVpnRouteStatus,
    clientVpnRouteStatus_code,
    clientVpnRouteStatus_message,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    newCoipAddressUsage,
    coipAddressUsage_allocationId,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,

    -- * CoipPool
    CoipPool (..),
    newCoipPool,
    coipPool_poolId,
    coipPool_localGatewayRouteTableId,
    coipPool_poolCidrs,
    coipPool_tags,
    coipPool_poolArn,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    newConnectionLogOptions,
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_cloudwatchLogGroup,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    newConnectionLogResponseOptions,
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_cloudwatchLogGroup,

    -- * ConnectionNotification
    ConnectionNotification (..),
    newConnectionNotification,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationType,
    connectionNotification_connectionEvents,
    connectionNotification_serviceId,
    connectionNotification_vpcEndpointId,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationArn,

    -- * ConversionTask
    ConversionTask (..),
    newConversionTask,
    conversionTask_importInstance,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_importVolume,
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
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
    createFleetError_lifecycle,
    createFleetError_launchTemplateAndOverrides,
    createFleetError_errorCode,
    createFleetError_errorMessage,

    -- * CreateFleetInstance
    CreateFleetInstance (..),
    newCreateFleetInstance,
    createFleetInstance_platform,
    createFleetInstance_lifecycle,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_instanceType,
    createFleetInstance_instanceIds,

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

    -- * CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (..),
    newCreateTransitGatewayVpcAttachmentRequestOptions,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,

    -- * CreateVolumePermission
    CreateVolumePermission (..),
    newCreateVolumePermission,
    createVolumePermission_group,
    createVolumePermission_userId,

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
    deleteFleetSuccessItem_previousFleetState,
    deleteFleetSuccessItem_fleetId,

    -- * DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    newDeleteLaunchTemplateVersionsResponseErrorItem,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,

    -- * DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    newDeleteLaunchTemplateVersionsResponseSuccessItem,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
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

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    newDescribeFastSnapshotRestoreSuccessItem,
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,

    -- * DescribeFleetError
    DescribeFleetError (..),
    newDescribeFleetError,
    describeFleetError_lifecycle,
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_errorCode,
    describeFleetError_errorMessage,

    -- * DescribeFleetsInstances
    DescribeFleetsInstances (..),
    newDescribeFleetsInstances,
    describeFleetsInstances_platform,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_instanceIds,

    -- * DestinationOptionsRequest
    DestinationOptionsRequest (..),
    newDestinationOptionsRequest,
    destinationOptionsRequest_perHourPartition,
    destinationOptionsRequest_fileFormat,
    destinationOptionsRequest_hiveCompatiblePartitions,

    -- * DestinationOptionsResponse
    DestinationOptionsResponse (..),
    newDestinationOptionsResponse,
    destinationOptionsResponse_perHourPartition,
    destinationOptionsResponse_fileFormat,
    destinationOptionsResponse_hiveCompatiblePartitions,

    -- * DhcpConfiguration
    DhcpConfiguration (..),
    newDhcpConfiguration,
    dhcpConfiguration_values,
    dhcpConfiguration_key,

    -- * DhcpOptions
    DhcpOptions (..),
    newDhcpOptions,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_ownerId,
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
    disableFastSnapshotRestoreStateError_code,
    disableFastSnapshotRestoreStateError_message,

    -- * DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    newDisableFastSnapshotRestoreStateErrorItem,
    disableFastSnapshotRestoreStateErrorItem_error,
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,

    -- * DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    newDisableFastSnapshotRestoreSuccessItem,
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,

    -- * DiskImage
    DiskImage (..),
    newDiskImage,
    diskImage_image,
    diskImage_volume,
    diskImage_description,

    -- * DiskImageDescription
    DiskImageDescription (..),
    newDiskImageDescription,
    diskImageDescription_size,
    diskImageDescription_checksum,
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,

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
    diskInfo_count,
    diskInfo_sizeInGB,
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
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_throughput,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_snapshotId,

    -- * EbsInfo
    EbsInfo (..),
    newEbsInfo,
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_encryptionSupport,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_nvmeSupport,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    newEbsInstanceBlockDevice,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_attachTime,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    newEbsInstanceBlockDeviceSpecification,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- * EbsOptimizedInfo
    EbsOptimizedInfo (..),
    newEbsOptimizedInfo,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- * EfaInfo
    EfaInfo (..),
    newEfaInfo,
    efaInfo_maximumEfaInterfaces,

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    newEgressOnlyInternetGateway,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_attachments,
    egressOnlyInternetGateway_tags,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    newElasticGpuAssociation,
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationId,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuAssociationState,

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
    elasticGpus_instanceId,
    elasticGpus_elasticGpuType,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuState,
    elasticGpus_elasticGpuHealth,
    elasticGpus_availabilityZone,
    elasticGpus_tags,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    newElasticInferenceAccelerator,
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    newElasticInferenceAcceleratorAssociation,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,

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
    enableFastSnapshotRestoreStateErrorItem_error,
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,

    -- * EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    newEnableFastSnapshotRestoreSuccessItem,
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,

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
    explanation_destination,
    explanation_state,
    explanation_cidrs,
    explanation_component,
    explanation_loadBalancerTargetGroups,
    explanation_securityGroups,
    explanation_prefixList,
    explanation_direction,
    explanation_protocols,
    explanation_loadBalancerListenerPort,
    explanation_portRanges,
    explanation_addresses,
    explanation_classicLoadBalancerListener,
    explanation_ingressRouteTable,
    explanation_networkInterface,
    explanation_loadBalancerTarget,
    explanation_subnet,
    explanation_natGateway,
    explanation_address,
    explanation_explanationCode,
    explanation_securityGroup,
    explanation_elasticLoadBalancerListener,
    explanation_loadBalancerTargetGroup,
    explanation_customerGateway,
    explanation_subnetRouteTable,
    explanation_availabilityZones,
    explanation_loadBalancerArn,
    explanation_routeTable,
    explanation_securityGroupRule,
    explanation_packetField,
    explanation_loadBalancerTargetPort,
    explanation_vpc,
    explanation_vpnGateway,
    explanation_sourceVpc,
    explanation_aclRule,
    explanation_internetGateway,
    explanation_missingComponent,
    explanation_acl,
    explanation_vpnConnection,
    explanation_routeTableRoute,
    explanation_vpcEndpoint,
    explanation_vpcPeeringConnection,
    explanation_port,
    explanation_destinationVpc,
    explanation_attachedTo,

    -- * ExportImageTask
    ExportImageTask (..),
    newExportImageTask,
    exportImageTask_status,
    exportImageTask_progress,
    exportImageTask_exportImageTaskId,
    exportImageTask_statusMessage,
    exportImageTask_imageId,
    exportImageTask_description,
    exportImageTask_tags,
    exportImageTask_s3ExportLocation,

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
    exportTaskS3Location_s3Prefix,
    exportTaskS3Location_s3Bucket,

    -- * ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (..),
    newExportTaskS3LocationRequest,
    exportTaskS3LocationRequest_s3Prefix,
    exportTaskS3LocationRequest_s3Bucket,

    -- * ExportToS3Task
    ExportToS3Task (..),
    newExportToS3Task,
    exportToS3Task_s3Key,
    exportToS3Task_containerFormat,
    exportToS3Task_s3Bucket,
    exportToS3Task_diskImageFormat,

    -- * ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    newExportToS3TaskSpecification,
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_s3Prefix,
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_diskImageFormat,

    -- * FailedCapacityReservationFleetCancellationResult
    FailedCapacityReservationFleetCancellationResult (..),
    newFailedCapacityReservationFleetCancellationResult,
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    newFailedQueuedPurchaseDeletion,
    failedQueuedPurchaseDeletion_error,
    failedQueuedPurchaseDeletion_reservedInstancesId,

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
    fleetCapacityReservation_priority,
    fleetCapacityReservation_availabilityZoneId,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_weight,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_totalInstanceCount,

    -- * FleetData
    FleetData (..),
    newFleetData,
    fleetData_context,
    fleetData_clientToken,
    fleetData_targetCapacitySpecification,
    fleetData_spotOptions,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_onDemandOptions,
    fleetData_fleetState,
    fleetData_launchTemplateConfigs,
    fleetData_validUntil,
    fleetData_terminateInstancesWithExpiration,
    fleetData_instances,
    fleetData_fulfilledCapacity,
    fleetData_type,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_fleetId,
    fleetData_errors,
    fleetData_createTime,
    fleetData_tags,
    fleetData_activityStatus,

    -- * FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (..),
    newFleetLaunchTemplateConfig,
    fleetLaunchTemplateConfig_overrides,
    fleetLaunchTemplateConfig_launchTemplateSpecification,

    -- * FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (..),
    newFleetLaunchTemplateConfigRequest,
    fleetLaunchTemplateConfigRequest_overrides,
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,

    -- * FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (..),
    newFleetLaunchTemplateOverrides,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_weightedCapacity,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_maxPrice,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    newFleetLaunchTemplateOverridesRequest,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_maxPrice,

    -- * FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    newFleetLaunchTemplateSpecification,
    fleetLaunchTemplateSpecification_launchTemplateName,
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_version,

    -- * FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    newFleetLaunchTemplateSpecificationRequest,
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
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
    flowLog_creationTime,
    flowLog_logFormat,
    flowLog_maxAggregationInterval,
    flowLog_resourceId,
    flowLog_flowLogStatus,
    flowLog_trafficType,
    flowLog_logDestination,
    flowLog_deliverLogsStatus,
    flowLog_deliverLogsErrorMessage,
    flowLog_logGroupName,
    flowLog_destinationOptions,
    flowLog_deliverLogsPermissionArn,
    flowLog_logDestinationType,
    flowLog_flowLogId,
    flowLog_tags,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    newFpgaDeviceInfo,
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_name,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    newFpgaDeviceMemoryInfo,
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- * FpgaImage
    FpgaImage (..),
    newFpgaImage,
    fpgaImage_shellVersion,
    fpgaImage_pciId,
    fpgaImage_state,
    fpgaImage_ownerAlias,
    fpgaImage_fpgaImageId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_ownerId,
    fpgaImage_updateTime,
    fpgaImage_name,
    fpgaImage_productCodes,
    fpgaImage_description,
    fpgaImage_createTime,
    fpgaImage_tags,
    fpgaImage_public,
    fpgaImage_fpgaImageGlobalId,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    newFpgaImageAttribute,
    fpgaImageAttribute_fpgaImageId,
    fpgaImageAttribute_name,
    fpgaImageAttribute_productCodes,
    fpgaImageAttribute_description,
    fpgaImageAttribute_loadPermissions,

    -- * FpgaImageState
    FpgaImageState (..),
    newFpgaImageState,
    fpgaImageState_code,
    fpgaImageState_message,

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
    gpuDeviceInfo_count,
    gpuDeviceInfo_name,

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
    host_releaseTime,
    host_state,
    host_clientToken,
    host_availabilityZoneId,
    host_hostId,
    host_availableCapacity,
    host_hostReservationId,
    host_allowsMultipleInstanceTypes,
    host_hostProperties,
    host_ownerId,
    host_availabilityZone,
    host_instances,
    host_allocationTime,
    host_memberOfServiceLinkedResourceGroup,
    host_hostRecovery,
    host_autoPlacement,
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
    hostOffering_instanceFamily,
    hostOffering_currencyCode,
    hostOffering_hourlyPrice,
    hostOffering_upfrontPrice,
    hostOffering_offeringId,
    hostOffering_duration,
    hostOffering_paymentOption,

    -- * HostProperties
    HostProperties (..),
    newHostProperties,
    hostProperties_instanceFamily,
    hostProperties_instanceType,
    hostProperties_totalVCpus,
    hostProperties_cores,
    hostProperties_sockets,

    -- * HostReservation
    HostReservation (..),
    newHostReservation,
    hostReservation_state,
    hostReservation_instanceFamily,
    hostReservation_currencyCode,
    hostReservation_hostReservationId,
    hostReservation_start,
    hostReservation_hourlyPrice,
    hostReservation_count,
    hostReservation_upfrontPrice,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_offeringId,
    hostReservation_duration,
    hostReservation_tags,
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
    iamInstanceProfileAssociation_associationId,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_iamInstanceProfile,
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
    idFormat_deadline,
    idFormat_resource,

    -- * Image
    Image (..),
    newImage,
    image_platform,
    image_platformDetails,
    image_deprecationTime,
    image_enaSupport,
    image_imageOwnerAlias,
    image_usageOperation,
    image_ramdiskId,
    image_kernelId,
    image_rootDeviceName,
    image_sriovNetSupport,
    image_name,
    image_bootMode,
    image_creationDate,
    image_productCodes,
    image_stateReason,
    image_description,
    image_blockDeviceMappings,
    image_tags,
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
    imageDiskContainer_url,
    imageDiskContainer_deviceName,
    imageDiskContainer_userBucket,
    imageDiskContainer_description,
    imageDiskContainer_snapshotId,

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
    importImageTask_status,
    importImageTask_hypervisor,
    importImageTask_platform,
    importImageTask_progress,
    importImageTask_licenseSpecifications,
    importImageTask_usageOperation,
    importImageTask_licenseType,
    importImageTask_snapshotDetails,
    importImageTask_encrypted,
    importImageTask_kmsKeyId,
    importImageTask_statusMessage,
    importImageTask_imageId,
    importImageTask_bootMode,
    importImageTask_importTaskId,
    importImageTask_architecture,
    importImageTask_description,
    importImageTask_tags,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    newImportInstanceLaunchSpecification,
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_placement,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    newImportInstanceTaskDetails,
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_description,

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    newImportInstanceVolumeDetailItem,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_description,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    newImportSnapshotTask,
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_importTaskId,
    importSnapshotTask_description,
    importSnapshotTask_tags,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    newImportVolumeTaskDetails,
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_volume,
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
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_name,

    -- * Instance
    Instance (..),
    newInstance,
    instance_publicDnsName,
    instance_platform,
    instance_securityGroups,
    instance_clientToken,
    instance_platformDetails,
    instance_enaSupport,
    instance_sourceDestCheck,
    instance_elasticGpuAssociations,
    instance_vpcId,
    instance_keyName,
    instance_networkInterfaces,
    instance_usageOperation,
    instance_outpostArn,
    instance_enclaveOptions,
    instance_ramdiskId,
    instance_cpuOptions,
    instance_subnetId,
    instance_kernelId,
    instance_rootDeviceName,
    instance_capacityReservationId,
    instance_capacityReservationSpecification,
    instance_sriovNetSupport,
    instance_ebsOptimized,
    instance_usageOperationUpdateTime,
    instance_stateTransitionReason,
    instance_hibernationOptions,
    instance_instanceLifecycle,
    instance_iamInstanceProfile,
    instance_privateIpAddress,
    instance_metadataOptions,
    instance_bootMode,
    instance_productCodes,
    instance_spotInstanceRequestId,
    instance_licenses,
    instance_elasticInferenceAcceleratorAssociations,
    instance_privateDnsName,
    instance_stateReason,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_tags,
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
    instanceBlockDeviceMappingSpecification_virtualName,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_deviceName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    newInstanceCapacity,
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,
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

    -- * InstanceEventWindow
    InstanceEventWindow (..),
    newInstanceEventWindow,
    instanceEventWindow_state,
    instanceEventWindow_associationTarget,
    instanceEventWindow_instanceEventWindowId,
    instanceEventWindow_name,
    instanceEventWindow_cronExpression,
    instanceEventWindow_tags,
    instanceEventWindow_timeRanges,

    -- * InstanceEventWindowAssociationRequest
    InstanceEventWindowAssociationRequest (..),
    newInstanceEventWindowAssociationRequest,
    instanceEventWindowAssociationRequest_instanceTags,
    instanceEventWindowAssociationRequest_instanceIds,
    instanceEventWindowAssociationRequest_dedicatedHostIds,

    -- * InstanceEventWindowAssociationTarget
    InstanceEventWindowAssociationTarget (..),
    newInstanceEventWindowAssociationTarget,
    instanceEventWindowAssociationTarget_instanceIds,
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_tags,

    -- * InstanceEventWindowDisassociationRequest
    InstanceEventWindowDisassociationRequest (..),
    newInstanceEventWindowDisassociationRequest,
    instanceEventWindowDisassociationRequest_instanceTags,
    instanceEventWindowDisassociationRequest_instanceIds,
    instanceEventWindowDisassociationRequest_dedicatedHostIds,

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

    -- * InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    newInstanceMarketOptionsRequest,
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- * InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    newInstanceMetadataOptionsRequest,
    instanceMetadataOptionsRequest_httpProtocolIpv6,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    newInstanceMetadataOptionsResponse,
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpProtocolIpv6,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
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
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_description,
    instanceNetworkInterface_association,
    instanceNetworkInterface_ipv6Addresses,

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    newInstanceNetworkInterfaceAssociation,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_publicIp,

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    newInstanceNetworkInterfaceAttachment,
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    newInstanceNetworkInterfaceSpecification,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_ipv6Addresses,

    -- * InstancePrivateIpAddress
    InstancePrivateIpAddress (..),
    newInstancePrivateIpAddress,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateIpAddress,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_association,

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
    instanceStatus_outpostArn,
    instanceStatus_systemStatus,
    instanceStatus_events,
    instanceStatus_availabilityZone,
    instanceStatus_instanceStatus,
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
    instanceStatusEvent_code,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_description,
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
    instanceStorageInfo_encryptionSupport,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_disks,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    newInstanceTagNotificationAttribute,
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    newInstanceTypeInfo,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_supportedBootModes,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_hibernationSupported,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    newInstanceTypeOffering,
    instanceTypeOffering_location,
    instanceTypeOffering_instanceType,
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
    ipPermission_userIdGroupPairs,
    ipPermission_prefixListIds,
    ipPermission_toPort,
    ipPermission_ipv6Ranges,
    ipPermission_ipRanges,
    ipPermission_ipProtocol,

    -- * IpRange
    IpRange (..),
    newIpRange,
    ipRange_description,
    ipRange_cidrIp,

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
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_poolId,
    ipv6Pool_description,
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
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyType,
    keyPairInfo_keyName,
    keyPairInfo_keyPairId,
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
    launchPermission_userId,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    newLaunchPermissionModifications,
    launchPermissionModifications_remove,
    launchPermissionModifications_add,

    -- * LaunchSpecification
    LaunchSpecification (..),
    newLaunchSpecification,
    launchSpecification_securityGroups,
    launchSpecification_keyName,
    launchSpecification_networkInterfaces,
    launchSpecification_ramdiskId,
    launchSpecification_subnetId,
    launchSpecification_kernelId,
    launchSpecification_instanceType,
    launchSpecification_ebsOptimized,
    launchSpecification_userData,
    launchSpecification_monitoring,
    launchSpecification_iamInstanceProfile,
    launchSpecification_imageId,
    launchSpecification_addressingType,
    launchSpecification_blockDeviceMappings,
    launchSpecification_placement,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_launchTemplateName,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_createTime,
    launchTemplate_tags,

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    newLaunchTemplateAndOverridesResponse,
    launchTemplateAndOverridesResponse_overrides,
    launchTemplateAndOverridesResponse_launchTemplateSpecification,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    newLaunchTemplateBlockDeviceMapping,
    launchTemplateBlockDeviceMapping_virtualName,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_deviceName,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    newLaunchTemplateBlockDeviceMappingRequest,
    launchTemplateBlockDeviceMappingRequest_virtualName,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_deviceName,

    -- * LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (..),
    newLaunchTemplateCapacityReservationSpecificationRequest,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference,

    -- * LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (..),
    newLaunchTemplateCapacityReservationSpecificationResponse,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference,

    -- * LaunchTemplateConfig
    LaunchTemplateConfig (..),
    newLaunchTemplateConfig,
    launchTemplateConfig_overrides,
    launchTemplateConfig_launchTemplateSpecification,

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
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_iops,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_snapshotId,

    -- * LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (..),
    newLaunchTemplateEbsBlockDeviceRequest,
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_iops,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_snapshotId,

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
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_httpTokens,

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    newLaunchTemplateInstanceMetadataOptionsRequest,
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecification,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,

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
    launchTemplateOverrides_priority,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_availabilityZone,

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    newLaunchTemplatePlacement,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_hostResourceGroupArn,

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    newLaunchTemplatePlacementRequest,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_hostResourceGroupArn,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
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
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_createTime,

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
    loadPermissionModifications_remove,
    loadPermissionModifications_add,

    -- * LoadPermissionRequest
    LoadPermissionRequest (..),
    newLoadPermissionRequest,
    loadPermissionRequest_group,
    loadPermissionRequest_userId,

    -- * LocalGateway
    LocalGateway (..),
    newLocalGateway,
    localGateway_state,
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_tags,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    newLocalGatewayRoute,
    localGatewayRoute_state,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_type,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_destinationCidrBlock,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    newLocalGatewayRouteTable,
    localGatewayRouteTable_state,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_tags,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- * LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (..),
    newLocalGatewayRouteTableVpcAssociation,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_vpcId,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_tags,

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    newLocalGatewayVirtualInterface,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_vlan,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_tags,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    newLocalGatewayVirtualInterfaceGroup,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_tags,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    newManagedPrefixList,
    managedPrefixList_stateMessage,
    managedPrefixList_state,
    managedPrefixList_prefixListArn,
    managedPrefixList_addressFamily,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListId,
    managedPrefixList_version,
    managedPrefixList_prefixListName,
    managedPrefixList_maxEntries,
    managedPrefixList_tags,

    -- * MemoryInfo
    MemoryInfo (..),
    newMemoryInfo,
    memoryInfo_sizeInMiB,

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    newModifyTransitGatewayOptions,
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_dnsSupport,

    -- * ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (..),
    newModifyTransitGatewayVpcAttachmentRequestOptions,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,

    -- * ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (..),
    newModifyVpnTunnelOptionsSpecification,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
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
    natGateway_state,
    natGateway_connectivityType,
    natGateway_failureCode,
    natGateway_vpcId,
    natGateway_failureMessage,
    natGateway_natGatewayId,
    natGateway_subnetId,
    natGateway_deleteTime,
    natGateway_provisionedBandwidth,
    natGateway_natGatewayAddresses,
    natGateway_createTime,
    natGateway_tags,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    newNatGatewayAddress,
    natGatewayAddress_privateIp,
    natGatewayAddress_allocationId,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_publicIp,

    -- * NetworkAcl
    NetworkAcl (..),
    newNetworkAcl,
    networkAcl_entries,
    networkAcl_networkAclId,
    networkAcl_vpcId,
    networkAcl_ownerId,
    networkAcl_associations,
    networkAcl_tags,
    networkAcl_isDefault,

    -- * NetworkAclAssociation
    NetworkAclAssociation (..),
    newNetworkAclAssociation,
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclAssociationId,

    -- * NetworkAclEntry
    NetworkAclEntry (..),
    newNetworkAclEntry,
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ruleNumber,
    networkAclEntry_ruleAction,
    networkAclEntry_protocol,
    networkAclEntry_portRange,
    networkAclEntry_cidrBlock,
    networkAclEntry_egress,

    -- * NetworkCardInfo
    NetworkCardInfo (..),
    newNetworkCardInfo,
    networkCardInfo_maximumNetworkInterfaces,
    networkCardInfo_networkPerformance,
    networkCardInfo_networkCardIndex,

    -- * NetworkInfo
    NetworkInfo (..),
    newNetworkInfo,
    networkInfo_efaSupported,
    networkInfo_ipv6Supported,
    networkInfo_enaSupport,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_networkPerformance,
    networkInfo_maximumNetworkCards,
    networkInfo_networkCards,
    networkInfo_efaInfo,
    networkInfo_defaultNetworkCardIndex,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_encryptionInTransitSupported,

    -- * NetworkInsightsAnalysis
    NetworkInsightsAnalysis (..),
    newNetworkInsightsAnalysis,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_tags,

    -- * NetworkInsightsPath
    NetworkInsightsPath (..),
    newNetworkInsightsPath,
    networkInsightsPath_destination,
    networkInsightsPath_destinationIp,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_protocol,
    networkInsightsPath_createdDate,
    networkInsightsPath_sourceIp,
    networkInsightsPath_source,
    networkInsightsPath_destinationPort,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_tags,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_groups,
    networkInterface_status,
    networkInterface_privateIpAddresses,
    networkInterface_ipv4Prefixes,
    networkInterface_sourceDestCheck,
    networkInterface_interfaceType,
    networkInterface_vpcId,
    networkInterface_tagSet,
    networkInterface_requesterManaged,
    networkInterface_outpostArn,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_macAddress,
    networkInterface_attachment,
    networkInterface_ownerId,
    networkInterface_ipv6Prefixes,
    networkInterface_availabilityZone,
    networkInterface_privateIpAddress,
    networkInterface_privateDnsName,
    networkInterface_requesterId,
    networkInterface_description,
    networkInterface_association,
    networkInterface_ipv6Addresses,

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    newNetworkInterfaceAssociation,
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_publicIp,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    newNetworkInterfaceAttachment,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_instanceOwnerId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_deviceIndex,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    newNetworkInterfaceAttachmentChanges,
    networkInterfaceAttachmentChanges_deleteOnTermination,
    networkInterfaceAttachmentChanges_attachmentId,

    -- * NetworkInterfaceIpv6Address
    NetworkInterfaceIpv6Address (..),
    newNetworkInterfaceIpv6Address,
    networkInterfaceIpv6Address_ipv6Address,

    -- * NetworkInterfacePermission
    NetworkInterfacePermission (..),
    newNetworkInterfacePermission,
    networkInterfacePermission_permissionState,
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_awsService,
    networkInterfacePermission_permission,

    -- * NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    newNetworkInterfacePermissionState,
    networkInterfacePermissionState_state,
    networkInterfacePermissionState_statusMessage,

    -- * NetworkInterfacePrivateIpAddress
    NetworkInterfacePrivateIpAddress (..),
    newNetworkInterfacePrivateIpAddress,
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_privateIpAddress,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_association,

    -- * NewDhcpConfiguration
    NewDhcpConfiguration (..),
    newNewDhcpConfiguration,
    newDhcpConfiguration_values,
    newDhcpConfiguration_key,

    -- * OnDemandOptions
    OnDemandOptions (..),
    newOnDemandOptions,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_singleInstanceType,
    onDemandOptions_allocationStrategy,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    newOnDemandOptionsRequest,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_allocationStrategy,

    -- * PathComponent
    PathComponent (..),
    newPathComponent,
    pathComponent_sequenceNumber,
    pathComponent_component,
    pathComponent_subnet,
    pathComponent_securityGroupRule,
    pathComponent_inboundHeader,
    pathComponent_vpc,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_outboundHeader,
    pathComponent_routeTableRoute,
    pathComponent_destinationVpc,

    -- * PciId
    PciId (..),
    newPciId,
    pciId_subsystemId,
    pciId_deviceId,
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
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,

    -- * PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    newPeeringConnectionOptionsRequest,
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,

    -- * PeeringTgwInfo
    PeeringTgwInfo (..),
    newPeeringTgwInfo,
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
    placement_affinity,
    placement_hostId,
    placement_partitionNumber,
    placement_spreadDomain,
    placement_availabilityZone,
    placement_tenancy,
    placement_groupName,
    placement_hostResourceGroupArn,

    -- * PlacementGroup
    PlacementGroup (..),
    newPlacementGroup,
    placementGroup_state,
    placementGroup_strategy,
    placementGroup_groupId,
    placementGroup_groupName,
    placementGroup_partitionCount,
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
    portRange_to,
    portRange_from,

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
    prefixListId_prefixListId,
    prefixListId_description,

    -- * PriceSchedule
    PriceSchedule (..),
    newPriceSchedule,
    priceSchedule_currencyCode,
    priceSchedule_term,
    priceSchedule_active,
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
    privateDnsNameConfiguration_value,
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_type,

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
    productCode_productCodeType,
    productCode_productCodeId,

    -- * PropagatingVgw
    PropagatingVgw (..),
    newPropagatingVgw,
    propagatingVgw_gatewayId,

    -- * ProvisionedBandwidth
    ProvisionedBandwidth (..),
    newProvisionedBandwidth,
    provisionedBandwidth_status,
    provisionedBandwidth_requested,
    provisionedBandwidth_provisioned,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_provisionTime,

    -- * PtrUpdateStatus
    PtrUpdateStatus (..),
    newPtrUpdateStatus,
    ptrUpdateStatus_status,
    ptrUpdateStatus_value,
    ptrUpdateStatus_reason,

    -- * PublicIpv4Pool
    PublicIpv4Pool (..),
    newPublicIpv4Pool,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_poolId,
    publicIpv4Pool_description,
    publicIpv4Pool_tags,

    -- * PublicIpv4PoolRange
    PublicIpv4PoolRange (..),
    newPublicIpv4PoolRange,
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_addressCount,

    -- * Purchase
    Purchase (..),
    newPurchase,
    purchase_instanceFamily,
    purchase_currencyCode,
    purchase_hostReservationId,
    purchase_hourlyPrice,
    purchase_upfrontPrice,
    purchase_hostIdSet,
    purchase_duration,
    purchase_paymentOption,

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
    referencedSecurityGroup_vpcPeeringConnectionId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_groupId,
    referencedSecurityGroup_peeringStatus,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_regionName,
    regionInfo_optInStatus,
    regionInfo_endpoint,

    -- * RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    newRegisterInstanceTagAttributeRequest,
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,
    registerInstanceTagAttributeRequest_instanceTagKeys,

    -- * RemovePrefixListEntry
    RemovePrefixListEntry (..),
    newRemovePrefixListEntry,
    removePrefixListEntry_cidr,

    -- * ReplaceRootVolumeTask
    ReplaceRootVolumeTask (..),
    newReplaceRootVolumeTask,
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_taskState,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_tags,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    newRequestLaunchTemplateData,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_metadataOptions,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_placement,

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    newRequestSpotLaunchSpecification,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_networkInterfaces,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_placement,

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
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_availabilityZoneId,
    reservationFleetInstanceSpecification_weight,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_availabilityZone,

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
    reservedInstances_state,
    reservedInstances_currencyCode,
    reservedInstances_instanceCount,
    reservedInstances_productDescription,
    reservedInstances_start,
    reservedInstances_instanceType,
    reservedInstances_end,
    reservedInstances_availabilityZone,
    reservedInstances_scope,
    reservedInstances_recurringCharges,
    reservedInstances_offeringType,
    reservedInstances_usagePrice,
    reservedInstances_fixedPrice,
    reservedInstances_reservedInstancesId,
    reservedInstances_instanceTenancy,
    reservedInstances_offeringClass,
    reservedInstances_duration,
    reservedInstances_tags,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    newReservedInstancesConfiguration,
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_instanceCount,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_scope,

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    newReservedInstancesId,
    reservedInstancesId_reservedInstancesId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    newReservedInstancesListing,
    reservedInstancesListing_status,
    reservedInstancesListing_clientToken,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_createDate,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_tags,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_reservedInstancesListingId,

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    newReservedInstancesModification,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_status,
    reservedInstancesModification_clientToken,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_createDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_reservedInstancesIds,

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    newReservedInstancesModificationResult,
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    newReservedInstancesOffering,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_usagePrice,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_duration,

    -- * ResponseError
    ResponseError (..),
    newResponseError,
    responseError_code,
    responseError_message,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    newResponseLaunchTemplateData,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_metadataOptions,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_placement,

    -- * Route
    Route (..),
    newRoute,
    route_vpcPeeringConnectionId,
    route_instanceId,
    route_origin,
    route_state,
    route_egressOnlyInternetGatewayId,
    route_destinationIpv6CidrBlock,
    route_localGatewayId,
    route_natGatewayId,
    route_networkInterfaceId,
    route_transitGatewayId,
    route_gatewayId,
    route_instanceOwnerId,
    route_destinationPrefixListId,
    route_carrierGatewayId,
    route_destinationCidrBlock,

    -- * RouteTable
    RouteTable (..),
    newRouteTable,
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_vpcId,
    routeTable_propagatingVgws,
    routeTable_ownerId,
    routeTable_associations,
    routeTable_tags,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    newRouteTableAssociation,
    routeTableAssociation_routeTableId,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_main,
    routeTableAssociation_subnetId,
    routeTableAssociation_gatewayId,
    routeTableAssociation_associationState,

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
    s3ObjectTag_value,
    s3ObjectTag_key,

    -- * S3Storage
    S3Storage (..),
    newS3Storage,
    s3Storage_prefix,
    s3Storage_uploadPolicy,
    s3Storage_bucket,
    s3Storage_uploadPolicySignature,
    s3Storage_aWSAccessKeyId,

    -- * ScheduledInstance
    ScheduledInstance (..),
    newScheduledInstance,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_platform,
    scheduledInstance_termStartDate,
    scheduledInstance_instanceCount,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_hourlyPrice,
    scheduledInstance_createDate,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_instanceType,
    scheduledInstance_recurrence,
    scheduledInstance_availabilityZone,
    scheduledInstance_termEndDate,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_networkPlatform,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    newScheduledInstanceAvailability,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_networkPlatform,

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    newScheduledInstanceRecurrence,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceDaySet,

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    newScheduledInstanceRecurrenceRequest,
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,
    scheduledInstanceRecurrenceRequest_interval,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    newScheduledInstancesBlockDeviceMapping,
    scheduledInstancesBlockDeviceMapping_virtualName,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_deviceName,

    -- * ScheduledInstancesEbs
    ScheduledInstancesEbs (..),
    newScheduledInstancesEbs,
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_iops,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_snapshotId,

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
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_imageId,

    -- * ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    newScheduledInstancesMonitoring,
    scheduledInstancesMonitoring_enabled,

    -- * ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    newScheduledInstancesNetworkInterface,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_ipv6Addresses,

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
    securityGroup_vpcId,
    securityGroup_ipPermissions,
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
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
    securityGroupReference_vpcPeeringConnectionId,
    securityGroupReference_referencingVpcId,
    securityGroupReference_groupId,

    -- * SecurityGroupRule
    SecurityGroupRule (..),
    newSecurityGroupRule,
    securityGroupRule_cidrIpv4,
    securityGroupRule_isEgress,
    securityGroupRule_fromPort,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_prefixListId,
    securityGroupRule_ipProtocol,
    securityGroupRule_groupId,
    securityGroupRule_toPort,
    securityGroupRule_cidrIpv6,
    securityGroupRule_description,
    securityGroupRule_tags,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_securityGroupRuleId,
    securityGroupRuleDescription_description,

    -- * SecurityGroupRuleRequest
    SecurityGroupRuleRequest (..),
    newSecurityGroupRuleRequest,
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_fromPort,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_toPort,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_description,

    -- * SecurityGroupRuleUpdate
    SecurityGroupRuleUpdate (..),
    newSecurityGroupRuleUpdate,
    securityGroupRuleUpdate_securityGroupRuleId,
    securityGroupRuleUpdate_securityGroupRule,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    newServiceConfiguration,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceType,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_serviceId,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_tags,

    -- * ServiceDetail
    ServiceDetail (..),
    newServiceDetail,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_owner,
    serviceDetail_availabilityZones,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_serviceName,
    serviceDetail_serviceType,
    serviceDetail_acceptanceRequired,
    serviceDetail_privateDnsNames,
    serviceDetail_serviceId,
    serviceDetail_privateDnsName,
    serviceDetail_tags,

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
    snapshot_stateMessage,
    snapshot_ownerAlias,
    snapshot_dataEncryptionKeyId,
    snapshot_outpostArn,
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
    snapshotDetail_status,
    snapshotDetail_progress,
    snapshotDetail_format,
    snapshotDetail_url,
    snapshotDetail_deviceName,
    snapshotDetail_statusMessage,
    snapshotDetail_userBucket,
    snapshotDetail_diskImageSize,
    snapshotDetail_description,
    snapshotDetail_snapshotId,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    newSnapshotDiskContainer,
    snapshotDiskContainer_format,
    snapshotDiskContainer_url,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_description,

    -- * SnapshotInfo
    SnapshotInfo (..),
    newSnapshotInfo,
    snapshotInfo_state,
    snapshotInfo_progress,
    snapshotInfo_startTime,
    snapshotInfo_volumeSize,
    snapshotInfo_outpostArn,
    snapshotInfo_encrypted,
    snapshotInfo_ownerId,
    snapshotInfo_volumeId,
    snapshotInfo_description,
    snapshotInfo_tags,
    snapshotInfo_snapshotId,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    newSnapshotTaskDetail,
    snapshotTaskDetail_status,
    snapshotTaskDetail_progress,
    snapshotTaskDetail_format,
    snapshotTaskDetail_url,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_description,
    snapshotTaskDetail_snapshotId,

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    newSpotCapacityRebalance,
    spotCapacityRebalance_replacementStrategy,

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    newSpotDatafeedSubscription,
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_fault,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    newSpotFleetLaunchSpecification,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_weightedCapacity,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_placement,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    newSpotFleetMonitoring,
    spotFleetMonitoring_enabled,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    newSpotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_activityStatus,

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    newSpotFleetRequestConfigData,
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_allocationStrategy,
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
    spotInstanceRequest_instanceId,
    spotInstanceRequest_status,
    spotInstanceRequest_state,
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_fault,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_type,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_createTime,
    spotInstanceRequest_tags,

    -- * SpotInstanceStateFault
    SpotInstanceStateFault (..),
    newSpotInstanceStateFault,
    spotInstanceStateFault_code,
    spotInstanceStateFault_message,

    -- * SpotInstanceStatus
    SpotInstanceStatus (..),
    newSpotInstanceStatus,
    spotInstanceStatus_updateTime,
    spotInstanceStatus_code,
    spotInstanceStatus_message,

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
    spotOptions_instanceInterruptionBehavior,
    spotOptions_singleAvailabilityZone,
    spotOptions_maxTotalPrice,
    spotOptions_minTargetCapacity,
    spotOptions_instancePoolsToUseCount,
    spotOptions_maintenanceStrategies,
    spotOptions_singleInstanceType,
    spotOptions_allocationStrategy,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    newSpotOptionsRequest,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_allocationStrategy,

    -- * SpotPlacement
    SpotPlacement (..),
    newSpotPlacement,
    spotPlacement_availabilityZone,
    spotPlacement_tenancy,
    spotPlacement_groupName,

    -- * SpotPrice
    SpotPrice (..),
    newSpotPrice,
    spotPrice_productDescription,
    spotPrice_spotPrice,
    spotPrice_instanceType,
    spotPrice_availabilityZone,
    spotPrice_timestamp,

    -- * StaleIpPermission
    StaleIpPermission (..),
    newStaleIpPermission,
    staleIpPermission_fromPort,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipProtocol,
    staleIpPermission_toPort,
    staleIpPermission_ipRanges,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    newStaleSecurityGroup,
    staleSecurityGroup_vpcId,
    staleSecurityGroup_groupId,
    staleSecurityGroup_groupName,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_description,

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
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_taskStartTime,
    storeImageTaskResult_bucket,
    storeImageTaskResult_progressPercentage,
    storeImageTaskResult_amiId,
    storeImageTaskResult_storeTaskFailureReason,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_availabilityZoneId,
    subnet_outpostArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_subnetArn,
    subnet_ownerId,
    subnet_customerOwnedIpv4Pool,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_mapPublicIpOnLaunch,
    subnet_defaultForAz,
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
    subnetCidrReservation_subnetId,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_cidr,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_description,
    subnetCidrReservation_tags,

    -- * SubnetIpv6CidrBlockAssociation
    SubnetIpv6CidrBlockAssociation (..),
    newSubnetIpv6CidrBlockAssociation,
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,

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
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    newTargetCapacitySpecificationRequest,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
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
    targetNetwork_associationId,
    targetNetwork_status,
    targetNetwork_securityGroups,
    targetNetwork_targetNetworkId,
    targetNetwork_vpcId,
    targetNetwork_clientVpnEndpointId,

    -- * TargetReservationValue
    TargetReservationValue (..),
    newTargetReservationValue,
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    newTerminateConnectionStatus,
    terminateConnectionStatus_currentStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_previousStatus,

    -- * TrafficMirrorFilter
    TrafficMirrorFilter (..),
    newTrafficMirrorFilter,
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_description,
    trafficMirrorFilter_tags,

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    newTrafficMirrorFilterRule,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_destinationCidrBlock,

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
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_virtualNetworkId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_description,
    trafficMirrorSession_tags,

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    newTrafficMirrorTarget,
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_type,
    trafficMirrorTarget_description,
    trafficMirrorTarget_tags,

    -- * TransitGateway
    TransitGateway (..),
    newTransitGateway,
    transitGateway_creationTime,
    transitGateway_state,
    transitGateway_ownerId,
    transitGateway_transitGatewayArn,
    transitGateway_transitGatewayId,
    transitGateway_options,
    transitGateway_description,
    transitGateway_tags,

    -- * TransitGatewayAssociation
    TransitGatewayAssociation (..),
    newTransitGatewayAssociation,
    transitGatewayAssociation_state,
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_transitGatewayRouteTableId,
    transitGatewayAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayAttachment
    TransitGatewayAttachment (..),
    newTransitGatewayAttachment,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_state,
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_transitGatewayOwnerId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_association,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    newTransitGatewayAttachmentAssociation,
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- * TransitGatewayAttachmentBgpConfiguration
    TransitGatewayAttachmentBgpConfiguration (..),
    newTransitGatewayAttachmentBgpConfiguration,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
    transitGatewayAttachmentBgpConfiguration_peerAddress,

    -- * TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    newTransitGatewayAttachmentPropagation,
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- * TransitGatewayConnect
    TransitGatewayConnect (..),
    newTransitGatewayConnect,
    transitGatewayConnect_creationTime,
    transitGatewayConnect_state,
    transitGatewayConnect_transportTransitGatewayAttachmentId,
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_options,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_tags,

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
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_tags,

    -- * TransitGatewayConnectPeerConfiguration
    TransitGatewayConnectPeerConfiguration (..),
    newTransitGatewayConnectPeerConfiguration,
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,

    -- * TransitGatewayConnectRequestBgpOptions
    TransitGatewayConnectRequestBgpOptions (..),
    newTransitGatewayConnectRequestBgpOptions,
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- * TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    newTransitGatewayMulticastDeregisteredGroupMembers,
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,

    -- * TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    newTransitGatewayMulticastDeregisteredGroupSources,
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,

    -- * TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    newTransitGatewayMulticastDomain,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_tags,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    newTransitGatewayMulticastDomainAssociation,
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    newTransitGatewayMulticastDomainAssociations,
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,

    -- * TransitGatewayMulticastDomainOptions
    TransitGatewayMulticastDomainOptions (..),
    newTransitGatewayMulticastDomainOptions,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    newTransitGatewayMulticastGroup,
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_resourceOwnerId,

    -- * TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    newTransitGatewayMulticastRegisteredGroupMembers,
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,

    -- * TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    newTransitGatewayMulticastRegisteredGroupSources,
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,

    -- * TransitGatewayOptions
    TransitGatewayOptions (..),
    newTransitGatewayOptions,
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_transitGatewayCidrBlocks,

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    newTransitGatewayPeeringAttachment,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,
    transitGatewayPeeringAttachment_tags,

    -- * TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (..),
    newTransitGatewayPrefixListAttachment,
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- * TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (..),
    newTransitGatewayPrefixListReference,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_transitGatewayAttachment,

    -- * TransitGatewayPropagation
    TransitGatewayPropagation (..),
    newTransitGatewayPropagation,
    transitGatewayPropagation_state,
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_transitGatewayRouteTableId,
    transitGatewayPropagation_transitGatewayAttachmentId,

    -- * TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    newTransitGatewayRequestOptions,
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    newTransitGatewayRoute,
    transitGatewayRoute_state,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_type,
    transitGatewayRoute_destinationCidrBlock,

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
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_transitGatewayRouteTableId,
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_tags,

    -- * TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (..),
    newTransitGatewayRouteTableAssociation,
    transitGatewayRouteTableAssociation_state,
    transitGatewayRouteTableAssociation_resourceId,
    transitGatewayRouteTableAssociation_resourceType,
    transitGatewayRouteTableAssociation_transitGatewayAttachmentId,

    -- * TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (..),
    newTransitGatewayRouteTablePropagation,
    transitGatewayRouteTablePropagation_state,
    transitGatewayRouteTablePropagation_resourceId,
    transitGatewayRouteTablePropagation_resourceType,
    transitGatewayRouteTablePropagation_transitGatewayAttachmentId,

    -- * TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (..),
    newTransitGatewayVpcAttachment,
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_vpcOwnerId,

    -- * TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (..),
    newTransitGatewayVpcAttachmentOptions,
    transitGatewayVpcAttachmentOptions_ipv6Support,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,

    -- * TrunkInterfaceAssociation
    TrunkInterfaceAssociation (..),
    newTrunkInterfaceAssociation,
    trunkInterfaceAssociation_associationId,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_vlanId,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_tags,

    -- * TunnelOption
    TunnelOption (..),
    newTunnelOption,
    tunnelOption_outsideIpAddress,
    tunnelOption_replayWindowSize,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_ikeVersions,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_startupAction,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_preSharedKey,
    tunnelOption_tunnelInsideIpv6Cidr,

    -- * UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    newUnsuccessfulInstanceCreditSpecificationItem,
    unsuccessfulInstanceCreditSpecificationItem_instanceId,
    unsuccessfulInstanceCreditSpecificationItem_error,

    -- * UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    newUnsuccessfulInstanceCreditSpecificationItemError,
    unsuccessfulInstanceCreditSpecificationItemError_code,
    unsuccessfulInstanceCreditSpecificationItemError_message,

    -- * UnsuccessfulItem
    UnsuccessfulItem (..),
    newUnsuccessfulItem,
    unsuccessfulItem_resourceId,
    unsuccessfulItem_error,

    -- * UnsuccessfulItemError
    UnsuccessfulItemError (..),
    newUnsuccessfulItemError,
    unsuccessfulItemError_code,
    unsuccessfulItemError_message,

    -- * UserBucket
    UserBucket (..),
    newUserBucket,
    userBucket_s3Key,
    userBucket_s3Bucket,

    -- * UserBucketDetails
    UserBucketDetails (..),
    newUserBucketDetails,
    userBucketDetails_s3Key,
    userBucketDetails_s3Bucket,

    -- * UserData
    UserData (..),
    newUserData,
    userData_data,

    -- * UserIdGroupPair
    UserIdGroupPair (..),
    newUserIdGroupPair,
    userIdGroupPair_vpcPeeringConnectionId,
    userIdGroupPair_vpcId,
    userIdGroupPair_userId,
    userIdGroupPair_groupId,
    userIdGroupPair_groupName,
    userIdGroupPair_description,
    userIdGroupPair_peeringStatus,

    -- * VCpuInfo
    VCpuInfo (..),
    newVCpuInfo,
    vCpuInfo_validThreadsPerCore,
    vCpuInfo_defaultThreadsPerCore,
    vCpuInfo_defaultVCpus,
    vCpuInfo_defaultCores,
    vCpuInfo_validCores,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_code,
    validationError_message,

    -- * ValidationWarning
    ValidationWarning (..),
    newValidationWarning,
    validationWarning_errors,

    -- * VgwTelemetry
    VgwTelemetry (..),
    newVgwTelemetry,
    vgwTelemetry_status,
    vgwTelemetry_outsideIpAddress,
    vgwTelemetry_certificateArn,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_statusMessage,

    -- * Volume
    Volume (..),
    newVolume,
    volume_fastRestored,
    volume_multiAttachEnabled,
    volume_attachments,
    volume_throughput,
    volume_iops,
    volume_outpostArn,
    volume_kmsKeyId,
    volume_tags,
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
    volumeAttachment_deleteOnTermination,
    volumeAttachment_state,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_attachTime,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_size,

    -- * VolumeModification
    VolumeModification (..),
    newVolumeModification,
    volumeModification_progress,
    volumeModification_startTime,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_modificationState,
    volumeModification_targetVolumeType,
    volumeModification_originalVolumeType,
    volumeModification_targetSize,
    volumeModification_targetIops,
    volumeModification_originalSize,
    volumeModification_originalIops,
    volumeModification_statusMessage,
    volumeModification_endTime,
    volumeModification_volumeId,
    volumeModification_originalThroughput,
    volumeModification_targetThroughput,

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
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventType,
    volumeStatusEvent_description,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_eventId,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    newVolumeStatusInfo,
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    newVolumeStatusItem,
    volumeStatusItem_volumeStatus,
    volumeStatusItem_actions,
    volumeStatusItem_outpostArn,
    volumeStatusItem_events,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_volumeId,
    volumeStatusItem_attachmentStatuses,

    -- * Vpc
    Vpc (..),
    newVpc,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_cidrBlockAssociationSet,
    vpc_ownerId,
    vpc_tags,
    vpc_isDefault,
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
    vpcCidrBlockAssociation_cidrBlockState,
    vpcCidrBlockAssociation_cidrBlock,

    -- * VpcCidrBlockState
    VpcCidrBlockState (..),
    newVpcCidrBlockState,
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- * VpcClassicLink
    VpcClassicLink (..),
    newVpcClassicLink,
    vpcClassicLink_vpcId,
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_groups,
    vpcEndpoint_state,
    vpcEndpoint_policyDocument,
    vpcEndpoint_subnetIds,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_vpcId,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_ownerId,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_serviceName,
    vpcEndpoint_lastError,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_tags,
    vpcEndpoint_routeTableIds,

    -- * VpcEndpointConnection
    VpcEndpointConnection (..),
    newVpcEndpointConnection,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_vpcEndpointId,

    -- * VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (..),
    newVpcIpv6CidrBlockAssociation,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_tags,

    -- * VpcPeeringConnectionOptionsDescription
    VpcPeeringConnectionOptionsDescription (..),
    newVpcPeeringConnectionOptionsDescription,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,

    -- * VpcPeeringConnectionStateReason
    VpcPeeringConnectionStateReason (..),
    newVpcPeeringConnectionStateReason,
    vpcPeeringConnectionStateReason_code,
    vpcPeeringConnectionStateReason_message,

    -- * VpcPeeringConnectionVpcInfo
    VpcPeeringConnectionVpcInfo (..),
    newVpcPeeringConnectionVpcInfo,
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_vpcId,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,

    -- * VpnConnection
    VpnConnection (..),
    newVpnConnection,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_routes,
    vpnConnection_vpnGatewayId,
    vpnConnection_category,
    vpnConnection_transitGatewayId,
    vpnConnection_options,
    vpnConnection_tags,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- * VpnConnectionDeviceType
    VpnConnectionDeviceType (..),
    newVpnConnectionDeviceType,
    vpnConnectionDeviceType_vendor,
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,
    vpnConnectionDeviceType_software,

    -- * VpnConnectionOptions
    VpnConnectionOptions (..),
    newVpnConnectionOptions,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,

    -- * VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (..),
    newVpnConnectionOptionsSpecification,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,

    -- * VpnGateway
    VpnGateway (..),
    newVpnGateway,
    vpnGateway_state,
    vpnGateway_vpcAttachments,
    vpnGateway_vpnGatewayId,
    vpnGateway_amazonSideAsn,
    vpnGateway_availabilityZone,
    vpnGateway_type,
    vpnGateway_tags,

    -- * VpnStaticRoute
    VpnStaticRoute (..),
    newVpnStaticRoute,
    vpnStaticRoute_state,
    vpnStaticRoute_source,
    vpnStaticRoute_destinationCidrBlock,

    -- * VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (..),
    newVpnTunnelOptionsSpecification,
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AccountAttribute
import Amazonka.EC2.Types.AccountAttributeName
import Amazonka.EC2.Types.AccountAttributeValue
import Amazonka.EC2.Types.ActiveInstance
import Amazonka.EC2.Types.ActivityStatus
import Amazonka.EC2.Types.AddPrefixListEntry
import Amazonka.EC2.Types.Address
import Amazonka.EC2.Types.AddressAttribute
import Amazonka.EC2.Types.AddressAttributeName
import Amazonka.EC2.Types.AddressStatus
import Amazonka.EC2.Types.Affinity
import Amazonka.EC2.Types.AllocationState
import Amazonka.EC2.Types.AllocationStrategy
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
import Amazonka.EC2.Types.BatchState
import Amazonka.EC2.Types.BgpStatus
import Amazonka.EC2.Types.BlobAttributeValue
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.BootModeType
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.BundleTask
import Amazonka.EC2.Types.BundleTaskError
import Amazonka.EC2.Types.BundleTaskState
import Amazonka.EC2.Types.ByoipCidr
import Amazonka.EC2.Types.ByoipCidrState
import Amazonka.EC2.Types.CancelBatchErrorCode
import Amazonka.EC2.Types.CancelCapacityReservationFleetError
import Amazonka.EC2.Types.CancelSpotFleetRequestsError
import Amazonka.EC2.Types.CancelSpotFleetRequestsErrorItem
import Amazonka.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Amazonka.EC2.Types.CancelSpotInstanceRequestState
import Amazonka.EC2.Types.CancelledSpotInstanceRequest
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
import Amazonka.EC2.Types.CoipAddressUsage
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
import Amazonka.EC2.Types.CpuOptions
import Amazonka.EC2.Types.CpuOptionsRequest
import Amazonka.EC2.Types.CreateFleetError
import Amazonka.EC2.Types.CreateFleetInstance
import Amazonka.EC2.Types.CreateTransitGatewayConnectRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayMulticastDomainRequestOptions
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
import Amazonka.EC2.Types.DnsServersOptionsModifyStructure
import Amazonka.EC2.Types.DnsSupportValue
import Amazonka.EC2.Types.DomainType
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
import Amazonka.EC2.Types.FastSnapshotRestoreStateCode
import Amazonka.EC2.Types.FederatedAuthentication
import Amazonka.EC2.Types.FederatedAuthenticationRequest
import Amazonka.EC2.Types.Filter
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
import Amazonka.EC2.Types.ImageState
import Amazonka.EC2.Types.ImageTypeValues
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
import Amazonka.EC2.Types.InstanceHealthStatus
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.InstanceIpv4Prefix
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.InstanceIpv6AddressRequest
import Amazonka.EC2.Types.InstanceIpv6Prefix
import Amazonka.EC2.Types.InstanceLifecycle
import Amazonka.EC2.Types.InstanceLifecycleType
import Amazonka.EC2.Types.InstanceMarketOptionsRequest
import Amazonka.EC2.Types.InstanceMatchCriteria
import Amazonka.EC2.Types.InstanceMetadataEndpointState
import Amazonka.EC2.Types.InstanceMetadataOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsResponse
import Amazonka.EC2.Types.InstanceMetadataOptionsState
import Amazonka.EC2.Types.InstanceMetadataProtocolState
import Amazonka.EC2.Types.InstanceMonitoring
import Amazonka.EC2.Types.InstanceNetworkInterface
import Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
import Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstancePrivateIpAddress
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
import Amazonka.EC2.Types.InstanceTypeOffering
import Amazonka.EC2.Types.InstanceUsage
import Amazonka.EC2.Types.IntegrateServices
import Amazonka.EC2.Types.InterfacePermissionType
import Amazonka.EC2.Types.InterfaceProtocolType
import Amazonka.EC2.Types.InternetGateway
import Amazonka.EC2.Types.InternetGatewayAttachment
import Amazonka.EC2.Types.IpPermission
import Amazonka.EC2.Types.IpRange
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
import Amazonka.EC2.Types.KeyPairInfo
import Amazonka.EC2.Types.KeyType
import Amazonka.EC2.Types.LastError
import Amazonka.EC2.Types.LaunchPermission
import Amazonka.EC2.Types.LaunchPermissionModifications
import Amazonka.EC2.Types.LaunchSpecification
import Amazonka.EC2.Types.LaunchTemplate
import Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
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
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataProtocolIpv6
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateLicenseConfiguration
import Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Amazonka.EC2.Types.LaunchTemplateOverrides
import Amazonka.EC2.Types.LaunchTemplatePlacement
import Amazonka.EC2.Types.LaunchTemplatePlacementRequest
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
import Amazonka.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Amazonka.EC2.Types.LocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.Types.LocalGatewayRouteType
import Amazonka.EC2.Types.LocalGatewayVirtualInterface
import Amazonka.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Amazonka.EC2.Types.LocationType
import Amazonka.EC2.Types.LogDestinationType
import Amazonka.EC2.Types.ManagedPrefixList
import Amazonka.EC2.Types.MarketType
import Amazonka.EC2.Types.MembershipType
import Amazonka.EC2.Types.MemoryInfo
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
import Amazonka.EC2.Types.NetworkCardInfo
import Amazonka.EC2.Types.NetworkInfo
import Amazonka.EC2.Types.NetworkInsightsAnalysis
import Amazonka.EC2.Types.NetworkInsightsPath
import Amazonka.EC2.Types.NetworkInterface
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import Amazonka.EC2.Types.NetworkInterfaceAttachment
import Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges
import Amazonka.EC2.Types.NetworkInterfaceAttribute
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
import Amazonka.EC2.Types.PartitionLoadFrequency
import Amazonka.EC2.Types.PathComponent
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
import Amazonka.EC2.Types.RemovePrefixListEntry
import Amazonka.EC2.Types.ReplaceRootVolumeTask
import Amazonka.EC2.Types.ReplaceRootVolumeTaskState
import Amazonka.EC2.Types.ReplacementStrategy
import Amazonka.EC2.Types.ReportInstanceReasonCodes
import Amazonka.EC2.Types.ReportStatusType
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
import Amazonka.EC2.Types.SnapshotState
import Amazonka.EC2.Types.SnapshotTaskDetail
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
import Amazonka.EC2.Types.SpotPrice
import Amazonka.EC2.Types.StaleIpPermission
import Amazonka.EC2.Types.StaleSecurityGroup
import Amazonka.EC2.Types.State
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.StaticSourcesSupportValue
import Amazonka.EC2.Types.StatusName
import Amazonka.EC2.Types.StatusType
import Amazonka.EC2.Types.Storage
import Amazonka.EC2.Types.StorageLocation
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
import Amazonka.EC2.Types.TargetConfiguration
import Amazonka.EC2.Types.TargetConfigurationRequest
import Amazonka.EC2.Types.TargetGroup
import Amazonka.EC2.Types.TargetGroupsConfig
import Amazonka.EC2.Types.TargetNetwork
import Amazonka.EC2.Types.TargetReservationValue
import Amazonka.EC2.Types.TelemetryStatus
import Amazonka.EC2.Types.Tenancy
import Amazonka.EC2.Types.TerminateConnectionStatus
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
import Amazonka.EC2.Types.TransitGatewayRouteTableAssociation
import Amazonka.EC2.Types.TransitGatewayRouteTablePropagation
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
import Amazonka.EC2.Types.VpnTunnelOptionsSpecification
import Amazonka.EC2.Types.WeekDay
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "EC2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ec2",
      Core._serviceSigningName = "ec2",
      Core._serviceVersion = "2016-11-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "EC2",
      Core._serviceRetry = retry
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
          ( Core.hasCode "EC2ThrottledException"
              Prelude.. Core.hasStatus 503
          )
          e =
        Prelude.Just "ec2_throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing
