{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.NetworkManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services enables you to centrally manage your Amazon Web
-- Services Cloud WAN core network and your Transit Gateway network across
-- Amazon Web Services accounts, Regions, and on-premises locations.
module Amazonka.NetworkManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** CoreNetworkPolicyException
    _CoreNetworkPolicyException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptAttachment
    AcceptAttachment (AcceptAttachment'),
    newAcceptAttachment,
    AcceptAttachmentResponse (AcceptAttachmentResponse'),
    newAcceptAttachmentResponse,

    -- ** AssociateConnectPeer
    AssociateConnectPeer (AssociateConnectPeer'),
    newAssociateConnectPeer,
    AssociateConnectPeerResponse (AssociateConnectPeerResponse'),
    newAssociateConnectPeerResponse,

    -- ** AssociateCustomerGateway
    AssociateCustomerGateway (AssociateCustomerGateway'),
    newAssociateCustomerGateway,
    AssociateCustomerGatewayResponse (AssociateCustomerGatewayResponse'),
    newAssociateCustomerGatewayResponse,

    -- ** AssociateLink
    AssociateLink (AssociateLink'),
    newAssociateLink,
    AssociateLinkResponse (AssociateLinkResponse'),
    newAssociateLinkResponse,

    -- ** AssociateTransitGatewayConnectPeer
    AssociateTransitGatewayConnectPeer (AssociateTransitGatewayConnectPeer'),
    newAssociateTransitGatewayConnectPeer,
    AssociateTransitGatewayConnectPeerResponse (AssociateTransitGatewayConnectPeerResponse'),
    newAssociateTransitGatewayConnectPeerResponse,

    -- ** CreateConnectAttachment
    CreateConnectAttachment (CreateConnectAttachment'),
    newCreateConnectAttachment,
    CreateConnectAttachmentResponse (CreateConnectAttachmentResponse'),
    newCreateConnectAttachmentResponse,

    -- ** CreateConnectPeer
    CreateConnectPeer (CreateConnectPeer'),
    newCreateConnectPeer,
    CreateConnectPeerResponse (CreateConnectPeerResponse'),
    newCreateConnectPeerResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** CreateCoreNetwork
    CreateCoreNetwork (CreateCoreNetwork'),
    newCreateCoreNetwork,
    CreateCoreNetworkResponse (CreateCoreNetworkResponse'),
    newCreateCoreNetworkResponse,

    -- ** CreateDevice
    CreateDevice (CreateDevice'),
    newCreateDevice,
    CreateDeviceResponse (CreateDeviceResponse'),
    newCreateDeviceResponse,

    -- ** CreateGlobalNetwork
    CreateGlobalNetwork (CreateGlobalNetwork'),
    newCreateGlobalNetwork,
    CreateGlobalNetworkResponse (CreateGlobalNetworkResponse'),
    newCreateGlobalNetworkResponse,

    -- ** CreateLink
    CreateLink (CreateLink'),
    newCreateLink,
    CreateLinkResponse (CreateLinkResponse'),
    newCreateLinkResponse,

    -- ** CreateSite
    CreateSite (CreateSite'),
    newCreateSite,
    CreateSiteResponse (CreateSiteResponse'),
    newCreateSiteResponse,

    -- ** CreateSiteToSiteVpnAttachment
    CreateSiteToSiteVpnAttachment (CreateSiteToSiteVpnAttachment'),
    newCreateSiteToSiteVpnAttachment,
    CreateSiteToSiteVpnAttachmentResponse (CreateSiteToSiteVpnAttachmentResponse'),
    newCreateSiteToSiteVpnAttachmentResponse,

    -- ** CreateTransitGatewayPeering
    CreateTransitGatewayPeering (CreateTransitGatewayPeering'),
    newCreateTransitGatewayPeering,
    CreateTransitGatewayPeeringResponse (CreateTransitGatewayPeeringResponse'),
    newCreateTransitGatewayPeeringResponse,

    -- ** CreateTransitGatewayRouteTableAttachment
    CreateTransitGatewayRouteTableAttachment (CreateTransitGatewayRouteTableAttachment'),
    newCreateTransitGatewayRouteTableAttachment,
    CreateTransitGatewayRouteTableAttachmentResponse (CreateTransitGatewayRouteTableAttachmentResponse'),
    newCreateTransitGatewayRouteTableAttachmentResponse,

    -- ** CreateVpcAttachment
    CreateVpcAttachment (CreateVpcAttachment'),
    newCreateVpcAttachment,
    CreateVpcAttachmentResponse (CreateVpcAttachmentResponse'),
    newCreateVpcAttachmentResponse,

    -- ** DeleteAttachment
    DeleteAttachment (DeleteAttachment'),
    newDeleteAttachment,
    DeleteAttachmentResponse (DeleteAttachmentResponse'),
    newDeleteAttachmentResponse,

    -- ** DeleteConnectPeer
    DeleteConnectPeer (DeleteConnectPeer'),
    newDeleteConnectPeer,
    DeleteConnectPeerResponse (DeleteConnectPeerResponse'),
    newDeleteConnectPeerResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DeleteCoreNetwork
    DeleteCoreNetwork (DeleteCoreNetwork'),
    newDeleteCoreNetwork,
    DeleteCoreNetworkResponse (DeleteCoreNetworkResponse'),
    newDeleteCoreNetworkResponse,

    -- ** DeleteCoreNetworkPolicyVersion
    DeleteCoreNetworkPolicyVersion (DeleteCoreNetworkPolicyVersion'),
    newDeleteCoreNetworkPolicyVersion,
    DeleteCoreNetworkPolicyVersionResponse (DeleteCoreNetworkPolicyVersionResponse'),
    newDeleteCoreNetworkPolicyVersionResponse,

    -- ** DeleteDevice
    DeleteDevice (DeleteDevice'),
    newDeleteDevice,
    DeleteDeviceResponse (DeleteDeviceResponse'),
    newDeleteDeviceResponse,

    -- ** DeleteGlobalNetwork
    DeleteGlobalNetwork (DeleteGlobalNetwork'),
    newDeleteGlobalNetwork,
    DeleteGlobalNetworkResponse (DeleteGlobalNetworkResponse'),
    newDeleteGlobalNetworkResponse,

    -- ** DeleteLink
    DeleteLink (DeleteLink'),
    newDeleteLink,
    DeleteLinkResponse (DeleteLinkResponse'),
    newDeleteLinkResponse,

    -- ** DeletePeering
    DeletePeering (DeletePeering'),
    newDeletePeering,
    DeletePeeringResponse (DeletePeeringResponse'),
    newDeletePeeringResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSite
    DeleteSite (DeleteSite'),
    newDeleteSite,
    DeleteSiteResponse (DeleteSiteResponse'),
    newDeleteSiteResponse,

    -- ** DeregisterTransitGateway
    DeregisterTransitGateway (DeregisterTransitGateway'),
    newDeregisterTransitGateway,
    DeregisterTransitGatewayResponse (DeregisterTransitGatewayResponse'),
    newDeregisterTransitGatewayResponse,

    -- ** DescribeGlobalNetworks (Paginated)
    DescribeGlobalNetworks (DescribeGlobalNetworks'),
    newDescribeGlobalNetworks,
    DescribeGlobalNetworksResponse (DescribeGlobalNetworksResponse'),
    newDescribeGlobalNetworksResponse,

    -- ** DisassociateConnectPeer
    DisassociateConnectPeer (DisassociateConnectPeer'),
    newDisassociateConnectPeer,
    DisassociateConnectPeerResponse (DisassociateConnectPeerResponse'),
    newDisassociateConnectPeerResponse,

    -- ** DisassociateCustomerGateway
    DisassociateCustomerGateway (DisassociateCustomerGateway'),
    newDisassociateCustomerGateway,
    DisassociateCustomerGatewayResponse (DisassociateCustomerGatewayResponse'),
    newDisassociateCustomerGatewayResponse,

    -- ** DisassociateLink
    DisassociateLink (DisassociateLink'),
    newDisassociateLink,
    DisassociateLinkResponse (DisassociateLinkResponse'),
    newDisassociateLinkResponse,

    -- ** DisassociateTransitGatewayConnectPeer
    DisassociateTransitGatewayConnectPeer (DisassociateTransitGatewayConnectPeer'),
    newDisassociateTransitGatewayConnectPeer,
    DisassociateTransitGatewayConnectPeerResponse (DisassociateTransitGatewayConnectPeerResponse'),
    newDisassociateTransitGatewayConnectPeerResponse,

    -- ** ExecuteCoreNetworkChangeSet
    ExecuteCoreNetworkChangeSet (ExecuteCoreNetworkChangeSet'),
    newExecuteCoreNetworkChangeSet,
    ExecuteCoreNetworkChangeSetResponse (ExecuteCoreNetworkChangeSetResponse'),
    newExecuteCoreNetworkChangeSetResponse,

    -- ** GetConnectAttachment
    GetConnectAttachment (GetConnectAttachment'),
    newGetConnectAttachment,
    GetConnectAttachmentResponse (GetConnectAttachmentResponse'),
    newGetConnectAttachmentResponse,

    -- ** GetConnectPeer
    GetConnectPeer (GetConnectPeer'),
    newGetConnectPeer,
    GetConnectPeerResponse (GetConnectPeerResponse'),
    newGetConnectPeerResponse,

    -- ** GetConnectPeerAssociations (Paginated)
    GetConnectPeerAssociations (GetConnectPeerAssociations'),
    newGetConnectPeerAssociations,
    GetConnectPeerAssociationsResponse (GetConnectPeerAssociationsResponse'),
    newGetConnectPeerAssociationsResponse,

    -- ** GetConnections (Paginated)
    GetConnections (GetConnections'),
    newGetConnections,
    GetConnectionsResponse (GetConnectionsResponse'),
    newGetConnectionsResponse,

    -- ** GetCoreNetwork
    GetCoreNetwork (GetCoreNetwork'),
    newGetCoreNetwork,
    GetCoreNetworkResponse (GetCoreNetworkResponse'),
    newGetCoreNetworkResponse,

    -- ** GetCoreNetworkChangeEvents (Paginated)
    GetCoreNetworkChangeEvents (GetCoreNetworkChangeEvents'),
    newGetCoreNetworkChangeEvents,
    GetCoreNetworkChangeEventsResponse (GetCoreNetworkChangeEventsResponse'),
    newGetCoreNetworkChangeEventsResponse,

    -- ** GetCoreNetworkChangeSet (Paginated)
    GetCoreNetworkChangeSet (GetCoreNetworkChangeSet'),
    newGetCoreNetworkChangeSet,
    GetCoreNetworkChangeSetResponse (GetCoreNetworkChangeSetResponse'),
    newGetCoreNetworkChangeSetResponse,

    -- ** GetCoreNetworkPolicy
    GetCoreNetworkPolicy (GetCoreNetworkPolicy'),
    newGetCoreNetworkPolicy,
    GetCoreNetworkPolicyResponse (GetCoreNetworkPolicyResponse'),
    newGetCoreNetworkPolicyResponse,

    -- ** GetCustomerGatewayAssociations (Paginated)
    GetCustomerGatewayAssociations (GetCustomerGatewayAssociations'),
    newGetCustomerGatewayAssociations,
    GetCustomerGatewayAssociationsResponse (GetCustomerGatewayAssociationsResponse'),
    newGetCustomerGatewayAssociationsResponse,

    -- ** GetDevices (Paginated)
    GetDevices (GetDevices'),
    newGetDevices,
    GetDevicesResponse (GetDevicesResponse'),
    newGetDevicesResponse,

    -- ** GetLinkAssociations (Paginated)
    GetLinkAssociations (GetLinkAssociations'),
    newGetLinkAssociations,
    GetLinkAssociationsResponse (GetLinkAssociationsResponse'),
    newGetLinkAssociationsResponse,

    -- ** GetLinks (Paginated)
    GetLinks (GetLinks'),
    newGetLinks,
    GetLinksResponse (GetLinksResponse'),
    newGetLinksResponse,

    -- ** GetNetworkResourceCounts (Paginated)
    GetNetworkResourceCounts (GetNetworkResourceCounts'),
    newGetNetworkResourceCounts,
    GetNetworkResourceCountsResponse (GetNetworkResourceCountsResponse'),
    newGetNetworkResourceCountsResponse,

    -- ** GetNetworkResourceRelationships (Paginated)
    GetNetworkResourceRelationships (GetNetworkResourceRelationships'),
    newGetNetworkResourceRelationships,
    GetNetworkResourceRelationshipsResponse (GetNetworkResourceRelationshipsResponse'),
    newGetNetworkResourceRelationshipsResponse,

    -- ** GetNetworkResources (Paginated)
    GetNetworkResources (GetNetworkResources'),
    newGetNetworkResources,
    GetNetworkResourcesResponse (GetNetworkResourcesResponse'),
    newGetNetworkResourcesResponse,

    -- ** GetNetworkRoutes
    GetNetworkRoutes (GetNetworkRoutes'),
    newGetNetworkRoutes,
    GetNetworkRoutesResponse (GetNetworkRoutesResponse'),
    newGetNetworkRoutesResponse,

    -- ** GetNetworkTelemetry (Paginated)
    GetNetworkTelemetry (GetNetworkTelemetry'),
    newGetNetworkTelemetry,
    GetNetworkTelemetryResponse (GetNetworkTelemetryResponse'),
    newGetNetworkTelemetryResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** GetRouteAnalysis
    GetRouteAnalysis (GetRouteAnalysis'),
    newGetRouteAnalysis,
    GetRouteAnalysisResponse (GetRouteAnalysisResponse'),
    newGetRouteAnalysisResponse,

    -- ** GetSiteToSiteVpnAttachment
    GetSiteToSiteVpnAttachment (GetSiteToSiteVpnAttachment'),
    newGetSiteToSiteVpnAttachment,
    GetSiteToSiteVpnAttachmentResponse (GetSiteToSiteVpnAttachmentResponse'),
    newGetSiteToSiteVpnAttachmentResponse,

    -- ** GetSites (Paginated)
    GetSites (GetSites'),
    newGetSites,
    GetSitesResponse (GetSitesResponse'),
    newGetSitesResponse,

    -- ** GetTransitGatewayConnectPeerAssociations (Paginated)
    GetTransitGatewayConnectPeerAssociations (GetTransitGatewayConnectPeerAssociations'),
    newGetTransitGatewayConnectPeerAssociations,
    GetTransitGatewayConnectPeerAssociationsResponse (GetTransitGatewayConnectPeerAssociationsResponse'),
    newGetTransitGatewayConnectPeerAssociationsResponse,

    -- ** GetTransitGatewayPeering
    GetTransitGatewayPeering (GetTransitGatewayPeering'),
    newGetTransitGatewayPeering,
    GetTransitGatewayPeeringResponse (GetTransitGatewayPeeringResponse'),
    newGetTransitGatewayPeeringResponse,

    -- ** GetTransitGatewayRegistrations (Paginated)
    GetTransitGatewayRegistrations (GetTransitGatewayRegistrations'),
    newGetTransitGatewayRegistrations,
    GetTransitGatewayRegistrationsResponse (GetTransitGatewayRegistrationsResponse'),
    newGetTransitGatewayRegistrationsResponse,

    -- ** GetTransitGatewayRouteTableAttachment
    GetTransitGatewayRouteTableAttachment (GetTransitGatewayRouteTableAttachment'),
    newGetTransitGatewayRouteTableAttachment,
    GetTransitGatewayRouteTableAttachmentResponse (GetTransitGatewayRouteTableAttachmentResponse'),
    newGetTransitGatewayRouteTableAttachmentResponse,

    -- ** GetVpcAttachment
    GetVpcAttachment (GetVpcAttachment'),
    newGetVpcAttachment,
    GetVpcAttachmentResponse (GetVpcAttachmentResponse'),
    newGetVpcAttachmentResponse,

    -- ** ListAttachments (Paginated)
    ListAttachments (ListAttachments'),
    newListAttachments,
    ListAttachmentsResponse (ListAttachmentsResponse'),
    newListAttachmentsResponse,

    -- ** ListConnectPeers (Paginated)
    ListConnectPeers (ListConnectPeers'),
    newListConnectPeers,
    ListConnectPeersResponse (ListConnectPeersResponse'),
    newListConnectPeersResponse,

    -- ** ListCoreNetworkPolicyVersions (Paginated)
    ListCoreNetworkPolicyVersions (ListCoreNetworkPolicyVersions'),
    newListCoreNetworkPolicyVersions,
    ListCoreNetworkPolicyVersionsResponse (ListCoreNetworkPolicyVersionsResponse'),
    newListCoreNetworkPolicyVersionsResponse,

    -- ** ListCoreNetworks (Paginated)
    ListCoreNetworks (ListCoreNetworks'),
    newListCoreNetworks,
    ListCoreNetworksResponse (ListCoreNetworksResponse'),
    newListCoreNetworksResponse,

    -- ** ListOrganizationServiceAccessStatus
    ListOrganizationServiceAccessStatus (ListOrganizationServiceAccessStatus'),
    newListOrganizationServiceAccessStatus,
    ListOrganizationServiceAccessStatusResponse (ListOrganizationServiceAccessStatusResponse'),
    newListOrganizationServiceAccessStatusResponse,

    -- ** ListPeerings (Paginated)
    ListPeerings (ListPeerings'),
    newListPeerings,
    ListPeeringsResponse (ListPeeringsResponse'),
    newListPeeringsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutCoreNetworkPolicy
    PutCoreNetworkPolicy (PutCoreNetworkPolicy'),
    newPutCoreNetworkPolicy,
    PutCoreNetworkPolicyResponse (PutCoreNetworkPolicyResponse'),
    newPutCoreNetworkPolicyResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RegisterTransitGateway
    RegisterTransitGateway (RegisterTransitGateway'),
    newRegisterTransitGateway,
    RegisterTransitGatewayResponse (RegisterTransitGatewayResponse'),
    newRegisterTransitGatewayResponse,

    -- ** RejectAttachment
    RejectAttachment (RejectAttachment'),
    newRejectAttachment,
    RejectAttachmentResponse (RejectAttachmentResponse'),
    newRejectAttachmentResponse,

    -- ** RestoreCoreNetworkPolicyVersion
    RestoreCoreNetworkPolicyVersion (RestoreCoreNetworkPolicyVersion'),
    newRestoreCoreNetworkPolicyVersion,
    RestoreCoreNetworkPolicyVersionResponse (RestoreCoreNetworkPolicyVersionResponse'),
    newRestoreCoreNetworkPolicyVersionResponse,

    -- ** StartOrganizationServiceAccessUpdate
    StartOrganizationServiceAccessUpdate (StartOrganizationServiceAccessUpdate'),
    newStartOrganizationServiceAccessUpdate,
    StartOrganizationServiceAccessUpdateResponse (StartOrganizationServiceAccessUpdateResponse'),
    newStartOrganizationServiceAccessUpdateResponse,

    -- ** StartRouteAnalysis
    StartRouteAnalysis (StartRouteAnalysis'),
    newStartRouteAnalysis,
    StartRouteAnalysisResponse (StartRouteAnalysisResponse'),
    newStartRouteAnalysisResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateConnection
    UpdateConnection (UpdateConnection'),
    newUpdateConnection,
    UpdateConnectionResponse (UpdateConnectionResponse'),
    newUpdateConnectionResponse,

    -- ** UpdateCoreNetwork
    UpdateCoreNetwork (UpdateCoreNetwork'),
    newUpdateCoreNetwork,
    UpdateCoreNetworkResponse (UpdateCoreNetworkResponse'),
    newUpdateCoreNetworkResponse,

    -- ** UpdateDevice
    UpdateDevice (UpdateDevice'),
    newUpdateDevice,
    UpdateDeviceResponse (UpdateDeviceResponse'),
    newUpdateDeviceResponse,

    -- ** UpdateGlobalNetwork
    UpdateGlobalNetwork (UpdateGlobalNetwork'),
    newUpdateGlobalNetwork,
    UpdateGlobalNetworkResponse (UpdateGlobalNetworkResponse'),
    newUpdateGlobalNetworkResponse,

    -- ** UpdateLink
    UpdateLink (UpdateLink'),
    newUpdateLink,
    UpdateLinkResponse (UpdateLinkResponse'),
    newUpdateLinkResponse,

    -- ** UpdateNetworkResourceMetadata
    UpdateNetworkResourceMetadata (UpdateNetworkResourceMetadata'),
    newUpdateNetworkResourceMetadata,
    UpdateNetworkResourceMetadataResponse (UpdateNetworkResourceMetadataResponse'),
    newUpdateNetworkResourceMetadataResponse,

    -- ** UpdateSite
    UpdateSite (UpdateSite'),
    newUpdateSite,
    UpdateSiteResponse (UpdateSiteResponse'),
    newUpdateSiteResponse,

    -- ** UpdateVpcAttachment
    UpdateVpcAttachment (UpdateVpcAttachment'),
    newUpdateVpcAttachment,
    UpdateVpcAttachmentResponse (UpdateVpcAttachmentResponse'),
    newUpdateVpcAttachmentResponse,

    -- * Types

    -- ** AttachmentState
    AttachmentState (..),

    -- ** AttachmentType
    AttachmentType (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeSetState
    ChangeSetState (..),

    -- ** ChangeStatus
    ChangeStatus (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** ConnectPeerAssociationState
    ConnectPeerAssociationState (..),

    -- ** ConnectPeerState
    ConnectPeerState (..),

    -- ** ConnectionState
    ConnectionState (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** CoreNetworkPolicyAlias
    CoreNetworkPolicyAlias (..),

    -- ** CoreNetworkState
    CoreNetworkState (..),

    -- ** CustomerGatewayAssociationState
    CustomerGatewayAssociationState (..),

    -- ** DeviceState
    DeviceState (..),

    -- ** GlobalNetworkState
    GlobalNetworkState (..),

    -- ** LinkAssociationState
    LinkAssociationState (..),

    -- ** LinkState
    LinkState (..),

    -- ** PeeringState
    PeeringState (..),

    -- ** PeeringType
    PeeringType (..),

    -- ** RouteAnalysisCompletionReasonCode
    RouteAnalysisCompletionReasonCode (..),

    -- ** RouteAnalysisCompletionResultCode
    RouteAnalysisCompletionResultCode (..),

    -- ** RouteAnalysisStatus
    RouteAnalysisStatus (..),

    -- ** RouteState
    RouteState (..),

    -- ** RouteTableType
    RouteTableType (..),

    -- ** RouteType
    RouteType (..),

    -- ** SiteState
    SiteState (..),

    -- ** TransitGatewayConnectPeerAssociationState
    TransitGatewayConnectPeerAssociationState (..),

    -- ** TransitGatewayRegistrationState
    TransitGatewayRegistrationState (..),

    -- ** TunnelProtocol
    TunnelProtocol (..),

    -- ** AWSLocation
    AWSLocation (AWSLocation'),
    newAWSLocation,

    -- ** AccountStatus
    AccountStatus (AccountStatus'),
    newAccountStatus,

    -- ** Attachment
    Attachment (Attachment'),
    newAttachment,

    -- ** Bandwidth
    Bandwidth (Bandwidth'),
    newBandwidth,

    -- ** BgpOptions
    BgpOptions (BgpOptions'),
    newBgpOptions,

    -- ** ConnectAttachment
    ConnectAttachment (ConnectAttachment'),
    newConnectAttachment,

    -- ** ConnectAttachmentOptions
    ConnectAttachmentOptions (ConnectAttachmentOptions'),
    newConnectAttachmentOptions,

    -- ** ConnectPeer
    ConnectPeer (ConnectPeer'),
    newConnectPeer,

    -- ** ConnectPeerAssociation
    ConnectPeerAssociation (ConnectPeerAssociation'),
    newConnectPeerAssociation,

    -- ** ConnectPeerBgpConfiguration
    ConnectPeerBgpConfiguration (ConnectPeerBgpConfiguration'),
    newConnectPeerBgpConfiguration,

    -- ** ConnectPeerConfiguration
    ConnectPeerConfiguration (ConnectPeerConfiguration'),
    newConnectPeerConfiguration,

    -- ** ConnectPeerSummary
    ConnectPeerSummary (ConnectPeerSummary'),
    newConnectPeerSummary,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** ConnectionHealth
    ConnectionHealth (ConnectionHealth'),
    newConnectionHealth,

    -- ** CoreNetwork
    CoreNetwork (CoreNetwork'),
    newCoreNetwork,

    -- ** CoreNetworkChange
    CoreNetworkChange (CoreNetworkChange'),
    newCoreNetworkChange,

    -- ** CoreNetworkChangeEvent
    CoreNetworkChangeEvent (CoreNetworkChangeEvent'),
    newCoreNetworkChangeEvent,

    -- ** CoreNetworkChangeEventValues
    CoreNetworkChangeEventValues (CoreNetworkChangeEventValues'),
    newCoreNetworkChangeEventValues,

    -- ** CoreNetworkChangeValues
    CoreNetworkChangeValues (CoreNetworkChangeValues'),
    newCoreNetworkChangeValues,

    -- ** CoreNetworkEdge
    CoreNetworkEdge (CoreNetworkEdge'),
    newCoreNetworkEdge,

    -- ** CoreNetworkPolicy
    CoreNetworkPolicy (CoreNetworkPolicy'),
    newCoreNetworkPolicy,

    -- ** CoreNetworkPolicyError
    CoreNetworkPolicyError (CoreNetworkPolicyError'),
    newCoreNetworkPolicyError,

    -- ** CoreNetworkPolicyVersion
    CoreNetworkPolicyVersion (CoreNetworkPolicyVersion'),
    newCoreNetworkPolicyVersion,

    -- ** CoreNetworkSegment
    CoreNetworkSegment (CoreNetworkSegment'),
    newCoreNetworkSegment,

    -- ** CoreNetworkSegmentEdgeIdentifier
    CoreNetworkSegmentEdgeIdentifier (CoreNetworkSegmentEdgeIdentifier'),
    newCoreNetworkSegmentEdgeIdentifier,

    -- ** CoreNetworkSummary
    CoreNetworkSummary (CoreNetworkSummary'),
    newCoreNetworkSummary,

    -- ** CustomerGatewayAssociation
    CustomerGatewayAssociation (CustomerGatewayAssociation'),
    newCustomerGatewayAssociation,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** GlobalNetwork
    GlobalNetwork (GlobalNetwork'),
    newGlobalNetwork,

    -- ** Link
    Link (Link'),
    newLink,

    -- ** LinkAssociation
    LinkAssociation (LinkAssociation'),
    newLinkAssociation,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** NetworkResource
    NetworkResource (NetworkResource'),
    newNetworkResource,

    -- ** NetworkResourceCount
    NetworkResourceCount (NetworkResourceCount'),
    newNetworkResourceCount,

    -- ** NetworkResourceSummary
    NetworkResourceSummary (NetworkResourceSummary'),
    newNetworkResourceSummary,

    -- ** NetworkRoute
    NetworkRoute (NetworkRoute'),
    newNetworkRoute,

    -- ** NetworkRouteDestination
    NetworkRouteDestination (NetworkRouteDestination'),
    newNetworkRouteDestination,

    -- ** NetworkTelemetry
    NetworkTelemetry (NetworkTelemetry'),
    newNetworkTelemetry,

    -- ** OrganizationStatus
    OrganizationStatus (OrganizationStatus'),
    newOrganizationStatus,

    -- ** PathComponent
    PathComponent (PathComponent'),
    newPathComponent,

    -- ** Peering
    Peering (Peering'),
    newPeering,

    -- ** ProposedSegmentChange
    ProposedSegmentChange (ProposedSegmentChange'),
    newProposedSegmentChange,

    -- ** Relationship
    Relationship (Relationship'),
    newRelationship,

    -- ** RouteAnalysis
    RouteAnalysis (RouteAnalysis'),
    newRouteAnalysis,

    -- ** RouteAnalysisCompletion
    RouteAnalysisCompletion (RouteAnalysisCompletion'),
    newRouteAnalysisCompletion,

    -- ** RouteAnalysisEndpointOptions
    RouteAnalysisEndpointOptions (RouteAnalysisEndpointOptions'),
    newRouteAnalysisEndpointOptions,

    -- ** RouteAnalysisEndpointOptionsSpecification
    RouteAnalysisEndpointOptionsSpecification (RouteAnalysisEndpointOptionsSpecification'),
    newRouteAnalysisEndpointOptionsSpecification,

    -- ** RouteAnalysisPath
    RouteAnalysisPath (RouteAnalysisPath'),
    newRouteAnalysisPath,

    -- ** RouteTableIdentifier
    RouteTableIdentifier (RouteTableIdentifier'),
    newRouteTableIdentifier,

    -- ** Site
    Site (Site'),
    newSite,

    -- ** SiteToSiteVpnAttachment
    SiteToSiteVpnAttachment (SiteToSiteVpnAttachment'),
    newSiteToSiteVpnAttachment,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TransitGatewayConnectPeerAssociation
    TransitGatewayConnectPeerAssociation (TransitGatewayConnectPeerAssociation'),
    newTransitGatewayConnectPeerAssociation,

    -- ** TransitGatewayPeering
    TransitGatewayPeering (TransitGatewayPeering'),
    newTransitGatewayPeering,

    -- ** TransitGatewayRegistration
    TransitGatewayRegistration (TransitGatewayRegistration'),
    newTransitGatewayRegistration,

    -- ** TransitGatewayRegistrationStateReason
    TransitGatewayRegistrationStateReason (TransitGatewayRegistrationStateReason'),
    newTransitGatewayRegistrationStateReason,

    -- ** TransitGatewayRouteTableAttachment
    TransitGatewayRouteTableAttachment (TransitGatewayRouteTableAttachment'),
    newTransitGatewayRouteTableAttachment,

    -- ** VpcAttachment
    VpcAttachment (VpcAttachment'),
    newVpcAttachment,

    -- ** VpcOptions
    VpcOptions (VpcOptions'),
    newVpcOptions,
  )
where

import Amazonka.NetworkManager.AcceptAttachment
import Amazonka.NetworkManager.AssociateConnectPeer
import Amazonka.NetworkManager.AssociateCustomerGateway
import Amazonka.NetworkManager.AssociateLink
import Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.CreateConnectAttachment
import Amazonka.NetworkManager.CreateConnectPeer
import Amazonka.NetworkManager.CreateConnection
import Amazonka.NetworkManager.CreateCoreNetwork
import Amazonka.NetworkManager.CreateDevice
import Amazonka.NetworkManager.CreateGlobalNetwork
import Amazonka.NetworkManager.CreateLink
import Amazonka.NetworkManager.CreateSite
import Amazonka.NetworkManager.CreateSiteToSiteVpnAttachment
import Amazonka.NetworkManager.CreateTransitGatewayPeering
import Amazonka.NetworkManager.CreateTransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.CreateVpcAttachment
import Amazonka.NetworkManager.DeleteAttachment
import Amazonka.NetworkManager.DeleteConnectPeer
import Amazonka.NetworkManager.DeleteConnection
import Amazonka.NetworkManager.DeleteCoreNetwork
import Amazonka.NetworkManager.DeleteCoreNetworkPolicyVersion
import Amazonka.NetworkManager.DeleteDevice
import Amazonka.NetworkManager.DeleteGlobalNetwork
import Amazonka.NetworkManager.DeleteLink
import Amazonka.NetworkManager.DeletePeering
import Amazonka.NetworkManager.DeleteResourcePolicy
import Amazonka.NetworkManager.DeleteSite
import Amazonka.NetworkManager.DeregisterTransitGateway
import Amazonka.NetworkManager.DescribeGlobalNetworks
import Amazonka.NetworkManager.DisassociateConnectPeer
import Amazonka.NetworkManager.DisassociateCustomerGateway
import Amazonka.NetworkManager.DisassociateLink
import Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.ExecuteCoreNetworkChangeSet
import Amazonka.NetworkManager.GetConnectAttachment
import Amazonka.NetworkManager.GetConnectPeer
import Amazonka.NetworkManager.GetConnectPeerAssociations
import Amazonka.NetworkManager.GetConnections
import Amazonka.NetworkManager.GetCoreNetwork
import Amazonka.NetworkManager.GetCoreNetworkChangeEvents
import Amazonka.NetworkManager.GetCoreNetworkChangeSet
import Amazonka.NetworkManager.GetCoreNetworkPolicy
import Amazonka.NetworkManager.GetCustomerGatewayAssociations
import Amazonka.NetworkManager.GetDevices
import Amazonka.NetworkManager.GetLinkAssociations
import Amazonka.NetworkManager.GetLinks
import Amazonka.NetworkManager.GetNetworkResourceCounts
import Amazonka.NetworkManager.GetNetworkResourceRelationships
import Amazonka.NetworkManager.GetNetworkResources
import Amazonka.NetworkManager.GetNetworkRoutes
import Amazonka.NetworkManager.GetNetworkTelemetry
import Amazonka.NetworkManager.GetResourcePolicy
import Amazonka.NetworkManager.GetRouteAnalysis
import Amazonka.NetworkManager.GetSiteToSiteVpnAttachment
import Amazonka.NetworkManager.GetSites
import Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
import Amazonka.NetworkManager.GetTransitGatewayPeering
import Amazonka.NetworkManager.GetTransitGatewayRegistrations
import Amazonka.NetworkManager.GetTransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.GetVpcAttachment
import Amazonka.NetworkManager.Lens
import Amazonka.NetworkManager.ListAttachments
import Amazonka.NetworkManager.ListConnectPeers
import Amazonka.NetworkManager.ListCoreNetworkPolicyVersions
import Amazonka.NetworkManager.ListCoreNetworks
import Amazonka.NetworkManager.ListOrganizationServiceAccessStatus
import Amazonka.NetworkManager.ListPeerings
import Amazonka.NetworkManager.ListTagsForResource
import Amazonka.NetworkManager.PutCoreNetworkPolicy
import Amazonka.NetworkManager.PutResourcePolicy
import Amazonka.NetworkManager.RegisterTransitGateway
import Amazonka.NetworkManager.RejectAttachment
import Amazonka.NetworkManager.RestoreCoreNetworkPolicyVersion
import Amazonka.NetworkManager.StartOrganizationServiceAccessUpdate
import Amazonka.NetworkManager.StartRouteAnalysis
import Amazonka.NetworkManager.TagResource
import Amazonka.NetworkManager.Types
import Amazonka.NetworkManager.UntagResource
import Amazonka.NetworkManager.UpdateConnection
import Amazonka.NetworkManager.UpdateCoreNetwork
import Amazonka.NetworkManager.UpdateDevice
import Amazonka.NetworkManager.UpdateGlobalNetwork
import Amazonka.NetworkManager.UpdateLink
import Amazonka.NetworkManager.UpdateNetworkResourceMetadata
import Amazonka.NetworkManager.UpdateSite
import Amazonka.NetworkManager.UpdateVpcAttachment
import Amazonka.NetworkManager.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'NetworkManager'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
