{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _CoreNetworkPolicyException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AttachmentState
    AttachmentState (..),

    -- * AttachmentType
    AttachmentType (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeSetState
    ChangeSetState (..),

    -- * ChangeStatus
    ChangeStatus (..),

    -- * ChangeType
    ChangeType (..),

    -- * ConnectPeerAssociationState
    ConnectPeerAssociationState (..),

    -- * ConnectPeerState
    ConnectPeerState (..),

    -- * ConnectionState
    ConnectionState (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * CoreNetworkPolicyAlias
    CoreNetworkPolicyAlias (..),

    -- * CoreNetworkState
    CoreNetworkState (..),

    -- * CustomerGatewayAssociationState
    CustomerGatewayAssociationState (..),

    -- * DeviceState
    DeviceState (..),

    -- * GlobalNetworkState
    GlobalNetworkState (..),

    -- * LinkAssociationState
    LinkAssociationState (..),

    -- * LinkState
    LinkState (..),

    -- * PeeringState
    PeeringState (..),

    -- * PeeringType
    PeeringType (..),

    -- * RouteAnalysisCompletionReasonCode
    RouteAnalysisCompletionReasonCode (..),

    -- * RouteAnalysisCompletionResultCode
    RouteAnalysisCompletionResultCode (..),

    -- * RouteAnalysisStatus
    RouteAnalysisStatus (..),

    -- * RouteState
    RouteState (..),

    -- * RouteTableType
    RouteTableType (..),

    -- * RouteType
    RouteType (..),

    -- * SiteState
    SiteState (..),

    -- * TransitGatewayConnectPeerAssociationState
    TransitGatewayConnectPeerAssociationState (..),

    -- * TransitGatewayRegistrationState
    TransitGatewayRegistrationState (..),

    -- * TunnelProtocol
    TunnelProtocol (..),

    -- * AWSLocation
    AWSLocation (..),
    newAWSLocation,
    aWSLocation_subnetArn,
    aWSLocation_zone,

    -- * AccountStatus
    AccountStatus (..),
    newAccountStatus,
    accountStatus_accountId,
    accountStatus_sLRDeploymentStatus,

    -- * Attachment
    Attachment (..),
    newAttachment,
    attachment_attachmentId,
    attachment_attachmentPolicyRuleNumber,
    attachment_attachmentType,
    attachment_coreNetworkArn,
    attachment_coreNetworkId,
    attachment_createdAt,
    attachment_edgeLocation,
    attachment_ownerAccountId,
    attachment_proposedSegmentChange,
    attachment_resourceArn,
    attachment_segmentName,
    attachment_state,
    attachment_tags,
    attachment_updatedAt,

    -- * Bandwidth
    Bandwidth (..),
    newBandwidth,
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

    -- * BgpOptions
    BgpOptions (..),
    newBgpOptions,
    bgpOptions_peerAsn,

    -- * ConnectAttachment
    ConnectAttachment (..),
    newConnectAttachment,
    connectAttachment_attachment,
    connectAttachment_options,
    connectAttachment_transportAttachmentId,

    -- * ConnectAttachmentOptions
    ConnectAttachmentOptions (..),
    newConnectAttachmentOptions,
    connectAttachmentOptions_protocol,

    -- * ConnectPeer
    ConnectPeer (..),
    newConnectPeer,
    connectPeer_configuration,
    connectPeer_connectAttachmentId,
    connectPeer_connectPeerId,
    connectPeer_coreNetworkId,
    connectPeer_createdAt,
    connectPeer_edgeLocation,
    connectPeer_state,
    connectPeer_tags,

    -- * ConnectPeerAssociation
    ConnectPeerAssociation (..),
    newConnectPeerAssociation,
    connectPeerAssociation_connectPeerId,
    connectPeerAssociation_deviceId,
    connectPeerAssociation_globalNetworkId,
    connectPeerAssociation_linkId,
    connectPeerAssociation_state,

    -- * ConnectPeerBgpConfiguration
    ConnectPeerBgpConfiguration (..),
    newConnectPeerBgpConfiguration,
    connectPeerBgpConfiguration_coreNetworkAddress,
    connectPeerBgpConfiguration_coreNetworkAsn,
    connectPeerBgpConfiguration_peerAddress,
    connectPeerBgpConfiguration_peerAsn,

    -- * ConnectPeerConfiguration
    ConnectPeerConfiguration (..),
    newConnectPeerConfiguration,
    connectPeerConfiguration_bgpConfigurations,
    connectPeerConfiguration_coreNetworkAddress,
    connectPeerConfiguration_insideCidrBlocks,
    connectPeerConfiguration_peerAddress,
    connectPeerConfiguration_protocol,

    -- * ConnectPeerSummary
    ConnectPeerSummary (..),
    newConnectPeerSummary,
    connectPeerSummary_connectAttachmentId,
    connectPeerSummary_connectPeerId,
    connectPeerSummary_connectPeerState,
    connectPeerSummary_coreNetworkId,
    connectPeerSummary_createdAt,
    connectPeerSummary_edgeLocation,
    connectPeerSummary_tags,

    -- * Connection
    Connection (..),
    newConnection,
    connection_connectedDeviceId,
    connection_connectedLinkId,
    connection_connectionArn,
    connection_connectionId,
    connection_createdAt,
    connection_description,
    connection_deviceId,
    connection_globalNetworkId,
    connection_linkId,
    connection_state,
    connection_tags,

    -- * ConnectionHealth
    ConnectionHealth (..),
    newConnectionHealth,
    connectionHealth_status,
    connectionHealth_timestamp,
    connectionHealth_type,

    -- * CoreNetwork
    CoreNetwork (..),
    newCoreNetwork,
    coreNetwork_coreNetworkArn,
    coreNetwork_coreNetworkId,
    coreNetwork_createdAt,
    coreNetwork_description,
    coreNetwork_edges,
    coreNetwork_globalNetworkId,
    coreNetwork_segments,
    coreNetwork_state,
    coreNetwork_tags,

    -- * CoreNetworkChange
    CoreNetworkChange (..),
    newCoreNetworkChange,
    coreNetworkChange_action,
    coreNetworkChange_identifier,
    coreNetworkChange_identifierPath,
    coreNetworkChange_newValues,
    coreNetworkChange_previousValues,
    coreNetworkChange_type,

    -- * CoreNetworkChangeEvent
    CoreNetworkChangeEvent (..),
    newCoreNetworkChangeEvent,
    coreNetworkChangeEvent_action,
    coreNetworkChangeEvent_eventTime,
    coreNetworkChangeEvent_identifierPath,
    coreNetworkChangeEvent_status,
    coreNetworkChangeEvent_type,
    coreNetworkChangeEvent_values,

    -- * CoreNetworkChangeEventValues
    CoreNetworkChangeEventValues (..),
    newCoreNetworkChangeEventValues,
    coreNetworkChangeEventValues_attachmentId,
    coreNetworkChangeEventValues_cidr,
    coreNetworkChangeEventValues_edgeLocation,
    coreNetworkChangeEventValues_segmentName,

    -- * CoreNetworkChangeValues
    CoreNetworkChangeValues (..),
    newCoreNetworkChangeValues,
    coreNetworkChangeValues_asn,
    coreNetworkChangeValues_cidr,
    coreNetworkChangeValues_destinationIdentifier,
    coreNetworkChangeValues_edgeLocations,
    coreNetworkChangeValues_insideCidrBlocks,
    coreNetworkChangeValues_segmentName,
    coreNetworkChangeValues_sharedSegments,

    -- * CoreNetworkEdge
    CoreNetworkEdge (..),
    newCoreNetworkEdge,
    coreNetworkEdge_asn,
    coreNetworkEdge_edgeLocation,
    coreNetworkEdge_insideCidrBlocks,

    -- * CoreNetworkPolicy
    CoreNetworkPolicy (..),
    newCoreNetworkPolicy,
    coreNetworkPolicy_alias,
    coreNetworkPolicy_changeSetState,
    coreNetworkPolicy_coreNetworkId,
    coreNetworkPolicy_createdAt,
    coreNetworkPolicy_description,
    coreNetworkPolicy_policyDocument,
    coreNetworkPolicy_policyErrors,
    coreNetworkPolicy_policyVersionId,

    -- * CoreNetworkPolicyError
    CoreNetworkPolicyError (..),
    newCoreNetworkPolicyError,
    coreNetworkPolicyError_path,
    coreNetworkPolicyError_errorCode,
    coreNetworkPolicyError_message,

    -- * CoreNetworkPolicyVersion
    CoreNetworkPolicyVersion (..),
    newCoreNetworkPolicyVersion,
    coreNetworkPolicyVersion_alias,
    coreNetworkPolicyVersion_changeSetState,
    coreNetworkPolicyVersion_coreNetworkId,
    coreNetworkPolicyVersion_createdAt,
    coreNetworkPolicyVersion_description,
    coreNetworkPolicyVersion_policyVersionId,

    -- * CoreNetworkSegment
    CoreNetworkSegment (..),
    newCoreNetworkSegment,
    coreNetworkSegment_edgeLocations,
    coreNetworkSegment_name,
    coreNetworkSegment_sharedSegments,

    -- * CoreNetworkSegmentEdgeIdentifier
    CoreNetworkSegmentEdgeIdentifier (..),
    newCoreNetworkSegmentEdgeIdentifier,
    coreNetworkSegmentEdgeIdentifier_coreNetworkId,
    coreNetworkSegmentEdgeIdentifier_edgeLocation,
    coreNetworkSegmentEdgeIdentifier_segmentName,

    -- * CoreNetworkSummary
    CoreNetworkSummary (..),
    newCoreNetworkSummary,
    coreNetworkSummary_coreNetworkArn,
    coreNetworkSummary_coreNetworkId,
    coreNetworkSummary_description,
    coreNetworkSummary_globalNetworkId,
    coreNetworkSummary_ownerAccountId,
    coreNetworkSummary_state,
    coreNetworkSummary_tags,

    -- * CustomerGatewayAssociation
    CustomerGatewayAssociation (..),
    newCustomerGatewayAssociation,
    customerGatewayAssociation_customerGatewayArn,
    customerGatewayAssociation_deviceId,
    customerGatewayAssociation_globalNetworkId,
    customerGatewayAssociation_linkId,
    customerGatewayAssociation_state,

    -- * Device
    Device (..),
    newDevice,
    device_aWSLocation,
    device_createdAt,
    device_description,
    device_deviceArn,
    device_deviceId,
    device_globalNetworkId,
    device_location,
    device_model,
    device_serialNumber,
    device_siteId,
    device_state,
    device_tags,
    device_type,
    device_vendor,

    -- * GlobalNetwork
    GlobalNetwork (..),
    newGlobalNetwork,
    globalNetwork_createdAt,
    globalNetwork_description,
    globalNetwork_globalNetworkArn,
    globalNetwork_globalNetworkId,
    globalNetwork_state,
    globalNetwork_tags,

    -- * Link
    Link (..),
    newLink,
    link_bandwidth,
    link_createdAt,
    link_description,
    link_globalNetworkId,
    link_linkArn,
    link_linkId,
    link_provider,
    link_siteId,
    link_state,
    link_tags,
    link_type,

    -- * LinkAssociation
    LinkAssociation (..),
    newLinkAssociation,
    linkAssociation_deviceId,
    linkAssociation_globalNetworkId,
    linkAssociation_linkAssociationState,
    linkAssociation_linkId,

    -- * Location
    Location (..),
    newLocation,
    location_address,
    location_latitude,
    location_longitude,

    -- * NetworkResource
    NetworkResource (..),
    newNetworkResource,
    networkResource_accountId,
    networkResource_awsRegion,
    networkResource_coreNetworkId,
    networkResource_definition,
    networkResource_definitionTimestamp,
    networkResource_metadata,
    networkResource_registeredGatewayArn,
    networkResource_resourceArn,
    networkResource_resourceId,
    networkResource_resourceType,
    networkResource_tags,

    -- * NetworkResourceCount
    NetworkResourceCount (..),
    newNetworkResourceCount,
    networkResourceCount_count,
    networkResourceCount_resourceType,

    -- * NetworkResourceSummary
    NetworkResourceSummary (..),
    newNetworkResourceSummary,
    networkResourceSummary_definition,
    networkResourceSummary_isMiddlebox,
    networkResourceSummary_nameTag,
    networkResourceSummary_registeredGatewayArn,
    networkResourceSummary_resourceArn,
    networkResourceSummary_resourceType,

    -- * NetworkRoute
    NetworkRoute (..),
    newNetworkRoute,
    networkRoute_destinationCidrBlock,
    networkRoute_destinations,
    networkRoute_prefixListId,
    networkRoute_state,
    networkRoute_type,

    -- * NetworkRouteDestination
    NetworkRouteDestination (..),
    newNetworkRouteDestination,
    networkRouteDestination_coreNetworkAttachmentId,
    networkRouteDestination_edgeLocation,
    networkRouteDestination_resourceId,
    networkRouteDestination_resourceType,
    networkRouteDestination_segmentName,
    networkRouteDestination_transitGatewayAttachmentId,

    -- * NetworkTelemetry
    NetworkTelemetry (..),
    newNetworkTelemetry,
    networkTelemetry_accountId,
    networkTelemetry_address,
    networkTelemetry_awsRegion,
    networkTelemetry_coreNetworkId,
    networkTelemetry_health,
    networkTelemetry_registeredGatewayArn,
    networkTelemetry_resourceArn,
    networkTelemetry_resourceId,
    networkTelemetry_resourceType,

    -- * OrganizationStatus
    OrganizationStatus (..),
    newOrganizationStatus,
    organizationStatus_accountStatusList,
    organizationStatus_organizationAwsServiceAccessStatus,
    organizationStatus_organizationId,
    organizationStatus_sLRDeploymentStatus,

    -- * PathComponent
    PathComponent (..),
    newPathComponent,
    pathComponent_destinationCidrBlock,
    pathComponent_resource,
    pathComponent_sequence,

    -- * Peering
    Peering (..),
    newPeering,
    peering_coreNetworkArn,
    peering_coreNetworkId,
    peering_createdAt,
    peering_edgeLocation,
    peering_ownerAccountId,
    peering_peeringId,
    peering_peeringType,
    peering_resourceArn,
    peering_state,
    peering_tags,

    -- * ProposedSegmentChange
    ProposedSegmentChange (..),
    newProposedSegmentChange,
    proposedSegmentChange_attachmentPolicyRuleNumber,
    proposedSegmentChange_segmentName,
    proposedSegmentChange_tags,

    -- * Relationship
    Relationship (..),
    newRelationship,
    relationship_from,
    relationship_to,

    -- * RouteAnalysis
    RouteAnalysis (..),
    newRouteAnalysis,
    routeAnalysis_destination,
    routeAnalysis_forwardPath,
    routeAnalysis_globalNetworkId,
    routeAnalysis_includeReturnPath,
    routeAnalysis_ownerAccountId,
    routeAnalysis_returnPath,
    routeAnalysis_routeAnalysisId,
    routeAnalysis_source,
    routeAnalysis_startTimestamp,
    routeAnalysis_status,
    routeAnalysis_useMiddleboxes,

    -- * RouteAnalysisCompletion
    RouteAnalysisCompletion (..),
    newRouteAnalysisCompletion,
    routeAnalysisCompletion_reasonCode,
    routeAnalysisCompletion_reasonContext,
    routeAnalysisCompletion_resultCode,

    -- * RouteAnalysisEndpointOptions
    RouteAnalysisEndpointOptions (..),
    newRouteAnalysisEndpointOptions,
    routeAnalysisEndpointOptions_ipAddress,
    routeAnalysisEndpointOptions_transitGatewayArn,
    routeAnalysisEndpointOptions_transitGatewayAttachmentArn,

    -- * RouteAnalysisEndpointOptionsSpecification
    RouteAnalysisEndpointOptionsSpecification (..),
    newRouteAnalysisEndpointOptionsSpecification,
    routeAnalysisEndpointOptionsSpecification_ipAddress,
    routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn,

    -- * RouteAnalysisPath
    RouteAnalysisPath (..),
    newRouteAnalysisPath,
    routeAnalysisPath_completionStatus,
    routeAnalysisPath_path,

    -- * RouteTableIdentifier
    RouteTableIdentifier (..),
    newRouteTableIdentifier,
    routeTableIdentifier_coreNetworkSegmentEdge,
    routeTableIdentifier_transitGatewayRouteTableArn,

    -- * Site
    Site (..),
    newSite,
    site_createdAt,
    site_description,
    site_globalNetworkId,
    site_location,
    site_siteArn,
    site_siteId,
    site_state,
    site_tags,

    -- * SiteToSiteVpnAttachment
    SiteToSiteVpnAttachment (..),
    newSiteToSiteVpnAttachment,
    siteToSiteVpnAttachment_attachment,
    siteToSiteVpnAttachment_vpnConnectionArn,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TransitGatewayConnectPeerAssociation
    TransitGatewayConnectPeerAssociation (..),
    newTransitGatewayConnectPeerAssociation,
    transitGatewayConnectPeerAssociation_deviceId,
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,

    -- * TransitGatewayPeering
    TransitGatewayPeering (..),
    newTransitGatewayPeering,
    transitGatewayPeering_peering,
    transitGatewayPeering_transitGatewayArn,
    transitGatewayPeering_transitGatewayPeeringAttachmentId,

    -- * TransitGatewayRegistration
    TransitGatewayRegistration (..),
    newTransitGatewayRegistration,
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_state,
    transitGatewayRegistration_transitGatewayArn,

    -- * TransitGatewayRegistrationStateReason
    TransitGatewayRegistrationStateReason (..),
    newTransitGatewayRegistrationStateReason,
    transitGatewayRegistrationStateReason_code,
    transitGatewayRegistrationStateReason_message,

    -- * TransitGatewayRouteTableAttachment
    TransitGatewayRouteTableAttachment (..),
    newTransitGatewayRouteTableAttachment,
    transitGatewayRouteTableAttachment_attachment,
    transitGatewayRouteTableAttachment_peeringId,
    transitGatewayRouteTableAttachment_transitGatewayRouteTableArn,

    -- * VpcAttachment
    VpcAttachment (..),
    newVpcAttachment,
    vpcAttachment_attachment,
    vpcAttachment_options,
    vpcAttachment_subnetArns,

    -- * VpcOptions
    VpcOptions (..),
    newVpcOptions,
    vpcOptions_ipv6Support,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.AWSLocation
import Amazonka.NetworkManager.Types.AccountStatus
import Amazonka.NetworkManager.Types.Attachment
import Amazonka.NetworkManager.Types.AttachmentState
import Amazonka.NetworkManager.Types.AttachmentType
import Amazonka.NetworkManager.Types.Bandwidth
import Amazonka.NetworkManager.Types.BgpOptions
import Amazonka.NetworkManager.Types.ChangeAction
import Amazonka.NetworkManager.Types.ChangeSetState
import Amazonka.NetworkManager.Types.ChangeStatus
import Amazonka.NetworkManager.Types.ChangeType
import Amazonka.NetworkManager.Types.ConnectAttachment
import Amazonka.NetworkManager.Types.ConnectAttachmentOptions
import Amazonka.NetworkManager.Types.ConnectPeer
import Amazonka.NetworkManager.Types.ConnectPeerAssociation
import Amazonka.NetworkManager.Types.ConnectPeerAssociationState
import Amazonka.NetworkManager.Types.ConnectPeerBgpConfiguration
import Amazonka.NetworkManager.Types.ConnectPeerConfiguration
import Amazonka.NetworkManager.Types.ConnectPeerState
import Amazonka.NetworkManager.Types.ConnectPeerSummary
import Amazonka.NetworkManager.Types.Connection
import Amazonka.NetworkManager.Types.ConnectionHealth
import Amazonka.NetworkManager.Types.ConnectionState
import Amazonka.NetworkManager.Types.ConnectionStatus
import Amazonka.NetworkManager.Types.ConnectionType
import Amazonka.NetworkManager.Types.CoreNetwork
import Amazonka.NetworkManager.Types.CoreNetworkChange
import Amazonka.NetworkManager.Types.CoreNetworkChangeEvent
import Amazonka.NetworkManager.Types.CoreNetworkChangeEventValues
import Amazonka.NetworkManager.Types.CoreNetworkChangeValues
import Amazonka.NetworkManager.Types.CoreNetworkEdge
import Amazonka.NetworkManager.Types.CoreNetworkPolicy
import Amazonka.NetworkManager.Types.CoreNetworkPolicyAlias
import Amazonka.NetworkManager.Types.CoreNetworkPolicyError
import Amazonka.NetworkManager.Types.CoreNetworkPolicyVersion
import Amazonka.NetworkManager.Types.CoreNetworkSegment
import Amazonka.NetworkManager.Types.CoreNetworkSegmentEdgeIdentifier
import Amazonka.NetworkManager.Types.CoreNetworkState
import Amazonka.NetworkManager.Types.CoreNetworkSummary
import Amazonka.NetworkManager.Types.CustomerGatewayAssociation
import Amazonka.NetworkManager.Types.CustomerGatewayAssociationState
import Amazonka.NetworkManager.Types.Device
import Amazonka.NetworkManager.Types.DeviceState
import Amazonka.NetworkManager.Types.GlobalNetwork
import Amazonka.NetworkManager.Types.GlobalNetworkState
import Amazonka.NetworkManager.Types.Link
import Amazonka.NetworkManager.Types.LinkAssociation
import Amazonka.NetworkManager.Types.LinkAssociationState
import Amazonka.NetworkManager.Types.LinkState
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.NetworkResource
import Amazonka.NetworkManager.Types.NetworkResourceCount
import Amazonka.NetworkManager.Types.NetworkResourceSummary
import Amazonka.NetworkManager.Types.NetworkRoute
import Amazonka.NetworkManager.Types.NetworkRouteDestination
import Amazonka.NetworkManager.Types.NetworkTelemetry
import Amazonka.NetworkManager.Types.OrganizationStatus
import Amazonka.NetworkManager.Types.PathComponent
import Amazonka.NetworkManager.Types.Peering
import Amazonka.NetworkManager.Types.PeeringState
import Amazonka.NetworkManager.Types.PeeringType
import Amazonka.NetworkManager.Types.ProposedSegmentChange
import Amazonka.NetworkManager.Types.Relationship
import Amazonka.NetworkManager.Types.RouteAnalysis
import Amazonka.NetworkManager.Types.RouteAnalysisCompletion
import Amazonka.NetworkManager.Types.RouteAnalysisCompletionReasonCode
import Amazonka.NetworkManager.Types.RouteAnalysisCompletionResultCode
import Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptions
import Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptionsSpecification
import Amazonka.NetworkManager.Types.RouteAnalysisPath
import Amazonka.NetworkManager.Types.RouteAnalysisStatus
import Amazonka.NetworkManager.Types.RouteState
import Amazonka.NetworkManager.Types.RouteTableIdentifier
import Amazonka.NetworkManager.Types.RouteTableType
import Amazonka.NetworkManager.Types.RouteType
import Amazonka.NetworkManager.Types.Site
import Amazonka.NetworkManager.Types.SiteState
import Amazonka.NetworkManager.Types.SiteToSiteVpnAttachment
import Amazonka.NetworkManager.Types.Tag
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociationState
import Amazonka.NetworkManager.Types.TransitGatewayPeering
import Amazonka.NetworkManager.Types.TransitGatewayRegistration
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason
import Amazonka.NetworkManager.Types.TransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.Types.TunnelProtocol
import Amazonka.NetworkManager.Types.VpcAttachment
import Amazonka.NetworkManager.Types.VpcOptions
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-07-05@ of the Amazon Network Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "NetworkManager",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "networkmanager",
      Core.signingName = "networkmanager",
      Core.version = "2019-07-05",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "NetworkManager",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict processing the request. Updating or deleting the
-- resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Describes a core network policy exception.
_CoreNetworkPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CoreNetworkPolicyException =
  Core._MatchServiceError
    defaultService
    "CoreNetworkPolicyException"
    Prelude.. Core.hasStatus 400

-- | The request has failed due to an internal error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A service limit was exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
