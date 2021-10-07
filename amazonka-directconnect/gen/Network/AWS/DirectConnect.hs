{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-10-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Direct Connect links your internal network to an Direct Connect location
-- over a standard Ethernet fiber-optic cable. One end of the cable is
-- connected to your router, the other to an Direct Connect router. With
-- this connection in place, you can create virtual interfaces directly to
-- the Cloud (for example, to Amazon EC2 and Amazon S3) and to Amazon VPC,
-- bypassing Internet service providers in your network path. A connection
-- provides access to all Regions except the China (Beijing) and (China)
-- Ningxia Regions. Amazon Web Services resources in the China Regions can
-- only be accessed through locations associated with those Regions.
module Network.AWS.DirectConnect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DirectConnectServerException
    _DirectConnectServerException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** DuplicateTagKeysException
    _DuplicateTagKeysException,

    -- ** DirectConnectClientException
    _DirectConnectClientException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateConnection
    UpdateConnection (UpdateConnection'),
    newUpdateConnection,
    Connection (Connection'),
    newConnection,

    -- ** StartBgpFailoverTest
    StartBgpFailoverTest (StartBgpFailoverTest'),
    newStartBgpFailoverTest,
    StartBgpFailoverTestResponse (StartBgpFailoverTestResponse'),
    newStartBgpFailoverTestResponse,

    -- ** DeleteDirectConnectGatewayAssociationProposal
    DeleteDirectConnectGatewayAssociationProposal (DeleteDirectConnectGatewayAssociationProposal'),
    newDeleteDirectConnectGatewayAssociationProposal,
    DeleteDirectConnectGatewayAssociationProposalResponse (DeleteDirectConnectGatewayAssociationProposalResponse'),
    newDeleteDirectConnectGatewayAssociationProposalResponse,

    -- ** DescribeVirtualGateways
    DescribeVirtualGateways (DescribeVirtualGateways'),
    newDescribeVirtualGateways,
    DescribeVirtualGatewaysResponse (DescribeVirtualGatewaysResponse'),
    newDescribeVirtualGatewaysResponse,

    -- ** StopBgpFailoverTest
    StopBgpFailoverTest (StopBgpFailoverTest'),
    newStopBgpFailoverTest,
    StopBgpFailoverTestResponse (StopBgpFailoverTestResponse'),
    newStopBgpFailoverTestResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    Connection (Connection'),
    newConnection,

    -- ** ConfirmPublicVirtualInterface
    ConfirmPublicVirtualInterface (ConfirmPublicVirtualInterface'),
    newConfirmPublicVirtualInterface,
    ConfirmPublicVirtualInterfaceResponse (ConfirmPublicVirtualInterfaceResponse'),
    newConfirmPublicVirtualInterfaceResponse,

    -- ** AllocatePrivateVirtualInterface
    AllocatePrivateVirtualInterface (AllocatePrivateVirtualInterface'),
    newAllocatePrivateVirtualInterface,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** DescribeDirectConnectGatewayAssociations (Paginated)
    DescribeDirectConnectGatewayAssociations (DescribeDirectConnectGatewayAssociations'),
    newDescribeDirectConnectGatewayAssociations,
    DescribeDirectConnectGatewayAssociationsResponse (DescribeDirectConnectGatewayAssociationsResponse'),
    newDescribeDirectConnectGatewayAssociationsResponse,

    -- ** ConfirmConnection
    ConfirmConnection (ConfirmConnection'),
    newConfirmConnection,
    ConfirmConnectionResponse (ConfirmConnectionResponse'),
    newConfirmConnectionResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeDirectConnectGatewayAttachments (Paginated)
    DescribeDirectConnectGatewayAttachments (DescribeDirectConnectGatewayAttachments'),
    newDescribeDirectConnectGatewayAttachments,
    DescribeDirectConnectGatewayAttachmentsResponse (DescribeDirectConnectGatewayAttachmentsResponse'),
    newDescribeDirectConnectGatewayAttachmentsResponse,

    -- ** CreatePublicVirtualInterface
    CreatePublicVirtualInterface (CreatePublicVirtualInterface'),
    newCreatePublicVirtualInterface,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** AssociateMacSecKey
    AssociateMacSecKey (AssociateMacSecKey'),
    newAssociateMacSecKey,
    AssociateMacSecKeyResponse (AssociateMacSecKeyResponse'),
    newAssociateMacSecKeyResponse,

    -- ** DescribeHostedConnections
    DescribeHostedConnections (DescribeHostedConnections'),
    newDescribeHostedConnections,
    Connections (Connections'),
    newConnections,

    -- ** AcceptDirectConnectGatewayAssociationProposal
    AcceptDirectConnectGatewayAssociationProposal (AcceptDirectConnectGatewayAssociationProposal'),
    newAcceptDirectConnectGatewayAssociationProposal,
    AcceptDirectConnectGatewayAssociationProposalResponse (AcceptDirectConnectGatewayAssociationProposalResponse'),
    newAcceptDirectConnectGatewayAssociationProposalResponse,

    -- ** CreateInterconnect
    CreateInterconnect (CreateInterconnect'),
    newCreateInterconnect,
    Interconnect (Interconnect'),
    newInterconnect,

    -- ** CreateDirectConnectGatewayAssociation
    CreateDirectConnectGatewayAssociation (CreateDirectConnectGatewayAssociation'),
    newCreateDirectConnectGatewayAssociation,
    CreateDirectConnectGatewayAssociationResponse (CreateDirectConnectGatewayAssociationResponse'),
    newCreateDirectConnectGatewayAssociationResponse,

    -- ** DeleteLag
    DeleteLag (DeleteLag'),
    newDeleteLag,
    Lag (Lag'),
    newLag,

    -- ** DeleteInterconnect
    DeleteInterconnect (DeleteInterconnect'),
    newDeleteInterconnect,
    DeleteInterconnectResponse (DeleteInterconnectResponse'),
    newDeleteInterconnectResponse,

    -- ** AssociateHostedConnection
    AssociateHostedConnection (AssociateHostedConnection'),
    newAssociateHostedConnection,
    Connection (Connection'),
    newConnection,

    -- ** CreateBGPPeer
    CreateBGPPeer (CreateBGPPeer'),
    newCreateBGPPeer,
    CreateBGPPeerResponse (CreateBGPPeerResponse'),
    newCreateBGPPeerResponse,

    -- ** UpdateLag
    UpdateLag (UpdateLag'),
    newUpdateLag,
    Lag (Lag'),
    newLag,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ConfirmPrivateVirtualInterface
    ConfirmPrivateVirtualInterface (ConfirmPrivateVirtualInterface'),
    newConfirmPrivateVirtualInterface,
    ConfirmPrivateVirtualInterfaceResponse (ConfirmPrivateVirtualInterfaceResponse'),
    newConfirmPrivateVirtualInterfaceResponse,

    -- ** DisassociateConnectionFromLag
    DisassociateConnectionFromLag (DisassociateConnectionFromLag'),
    newDisassociateConnectionFromLag,
    Connection (Connection'),
    newConnection,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteVirtualInterface
    DeleteVirtualInterface (DeleteVirtualInterface'),
    newDeleteVirtualInterface,
    DeleteVirtualInterfaceResponse (DeleteVirtualInterfaceResponse'),
    newDeleteVirtualInterfaceResponse,

    -- ** DescribeDirectConnectGateways (Paginated)
    DescribeDirectConnectGateways (DescribeDirectConnectGateways'),
    newDescribeDirectConnectGateways,
    DescribeDirectConnectGatewaysResponse (DescribeDirectConnectGatewaysResponse'),
    newDescribeDirectConnectGatewaysResponse,

    -- ** DescribeVirtualInterfaces
    DescribeVirtualInterfaces (DescribeVirtualInterfaces'),
    newDescribeVirtualInterfaces,
    DescribeVirtualInterfacesResponse (DescribeVirtualInterfacesResponse'),
    newDescribeVirtualInterfacesResponse,

    -- ** UpdateVirtualInterfaceAttributes
    UpdateVirtualInterfaceAttributes (UpdateVirtualInterfaceAttributes'),
    newUpdateVirtualInterfaceAttributes,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    Connection (Connection'),
    newConnection,

    -- ** AssociateConnectionWithLag
    AssociateConnectionWithLag (AssociateConnectionWithLag'),
    newAssociateConnectionWithLag,
    Connection (Connection'),
    newConnection,

    -- ** ListVirtualInterfaceTestHistory
    ListVirtualInterfaceTestHistory (ListVirtualInterfaceTestHistory'),
    newListVirtualInterfaceTestHistory,
    ListVirtualInterfaceTestHistoryResponse (ListVirtualInterfaceTestHistoryResponse'),
    newListVirtualInterfaceTestHistoryResponse,

    -- ** DisassociateMacSecKey
    DisassociateMacSecKey (DisassociateMacSecKey'),
    newDisassociateMacSecKey,
    DisassociateMacSecKeyResponse (DisassociateMacSecKeyResponse'),
    newDisassociateMacSecKeyResponse,

    -- ** DescribeLoa
    DescribeLoa (DescribeLoa'),
    newDescribeLoa,
    DescribeLoaResponse (DescribeLoaResponse'),
    newDescribeLoaResponse,

    -- ** CreateTransitVirtualInterface
    CreateTransitVirtualInterface (CreateTransitVirtualInterface'),
    newCreateTransitVirtualInterface,
    CreateTransitVirtualInterfaceResponse (CreateTransitVirtualInterfaceResponse'),
    newCreateTransitVirtualInterfaceResponse,

    -- ** CreateDirectConnectGateway
    CreateDirectConnectGateway (CreateDirectConnectGateway'),
    newCreateDirectConnectGateway,
    CreateDirectConnectGatewayResponse (CreateDirectConnectGatewayResponse'),
    newCreateDirectConnectGatewayResponse,

    -- ** DescribeInterconnects
    DescribeInterconnects (DescribeInterconnects'),
    newDescribeInterconnects,
    DescribeInterconnectsResponse (DescribeInterconnectsResponse'),
    newDescribeInterconnectsResponse,

    -- ** DescribeLags
    DescribeLags (DescribeLags'),
    newDescribeLags,
    DescribeLagsResponse (DescribeLagsResponse'),
    newDescribeLagsResponse,

    -- ** ConfirmTransitVirtualInterface
    ConfirmTransitVirtualInterface (ConfirmTransitVirtualInterface'),
    newConfirmTransitVirtualInterface,
    ConfirmTransitVirtualInterfaceResponse (ConfirmTransitVirtualInterfaceResponse'),
    newConfirmTransitVirtualInterfaceResponse,

    -- ** CreateLag
    CreateLag (CreateLag'),
    newCreateLag,
    Lag (Lag'),
    newLag,

    -- ** DeleteBGPPeer
    DeleteBGPPeer (DeleteBGPPeer'),
    newDeleteBGPPeer,
    DeleteBGPPeerResponse (DeleteBGPPeerResponse'),
    newDeleteBGPPeerResponse,

    -- ** AllocateTransitVirtualInterface
    AllocateTransitVirtualInterface (AllocateTransitVirtualInterface'),
    newAllocateTransitVirtualInterface,
    AllocateTransitVirtualInterfaceResponse (AllocateTransitVirtualInterfaceResponse'),
    newAllocateTransitVirtualInterfaceResponse,

    -- ** DeleteDirectConnectGatewayAssociation
    DeleteDirectConnectGatewayAssociation (DeleteDirectConnectGatewayAssociation'),
    newDeleteDirectConnectGatewayAssociation,
    DeleteDirectConnectGatewayAssociationResponse (DeleteDirectConnectGatewayAssociationResponse'),
    newDeleteDirectConnectGatewayAssociationResponse,

    -- ** UpdateDirectConnectGatewayAssociation
    UpdateDirectConnectGatewayAssociation (UpdateDirectConnectGatewayAssociation'),
    newUpdateDirectConnectGatewayAssociation,
    UpdateDirectConnectGatewayAssociationResponse (UpdateDirectConnectGatewayAssociationResponse'),
    newUpdateDirectConnectGatewayAssociationResponse,

    -- ** DescribeLocations
    DescribeLocations (DescribeLocations'),
    newDescribeLocations,
    DescribeLocationsResponse (DescribeLocationsResponse'),
    newDescribeLocationsResponse,

    -- ** DescribeConnections
    DescribeConnections (DescribeConnections'),
    newDescribeConnections,
    Connections (Connections'),
    newConnections,

    -- ** AllocatePublicVirtualInterface
    AllocatePublicVirtualInterface (AllocatePublicVirtualInterface'),
    newAllocatePublicVirtualInterface,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** AssociateVirtualInterface
    AssociateVirtualInterface (AssociateVirtualInterface'),
    newAssociateVirtualInterface,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** DescribeDirectConnectGatewayAssociationProposals
    DescribeDirectConnectGatewayAssociationProposals (DescribeDirectConnectGatewayAssociationProposals'),
    newDescribeDirectConnectGatewayAssociationProposals,
    DescribeDirectConnectGatewayAssociationProposalsResponse (DescribeDirectConnectGatewayAssociationProposalsResponse'),
    newDescribeDirectConnectGatewayAssociationProposalsResponse,

    -- ** CreatePrivateVirtualInterface
    CreatePrivateVirtualInterface (CreatePrivateVirtualInterface'),
    newCreatePrivateVirtualInterface,
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** CreateDirectConnectGatewayAssociationProposal
    CreateDirectConnectGatewayAssociationProposal (CreateDirectConnectGatewayAssociationProposal'),
    newCreateDirectConnectGatewayAssociationProposal,
    CreateDirectConnectGatewayAssociationProposalResponse (CreateDirectConnectGatewayAssociationProposalResponse'),
    newCreateDirectConnectGatewayAssociationProposalResponse,

    -- ** DeleteDirectConnectGateway
    DeleteDirectConnectGateway (DeleteDirectConnectGateway'),
    newDeleteDirectConnectGateway,
    DeleteDirectConnectGatewayResponse (DeleteDirectConnectGatewayResponse'),
    newDeleteDirectConnectGatewayResponse,

    -- ** AllocateHostedConnection
    AllocateHostedConnection (AllocateHostedConnection'),
    newAllocateHostedConnection,
    Connection (Connection'),
    newConnection,

    -- * Types

    -- ** AddressFamily
    AddressFamily (..),

    -- ** BGPPeerState
    BGPPeerState (..),

    -- ** BGPStatus
    BGPStatus (..),

    -- ** ConnectionState
    ConnectionState (..),

    -- ** DirectConnectGatewayAssociationProposalState
    DirectConnectGatewayAssociationProposalState (..),

    -- ** DirectConnectGatewayAssociationState
    DirectConnectGatewayAssociationState (..),

    -- ** DirectConnectGatewayAttachmentState
    DirectConnectGatewayAttachmentState (..),

    -- ** DirectConnectGatewayAttachmentType
    DirectConnectGatewayAttachmentType (..),

    -- ** DirectConnectGatewayState
    DirectConnectGatewayState (..),

    -- ** GatewayType
    GatewayType (..),

    -- ** HasLogicalRedundancy
    HasLogicalRedundancy (..),

    -- ** InterconnectState
    InterconnectState (..),

    -- ** LagState
    LagState (..),

    -- ** LoaContentType
    LoaContentType (..),

    -- ** VirtualInterfaceState
    VirtualInterfaceState (..),

    -- ** AssociatedGateway
    AssociatedGateway (AssociatedGateway'),
    newAssociatedGateway,

    -- ** BGPPeer
    BGPPeer (BGPPeer'),
    newBGPPeer,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** Connections
    Connections (Connections'),
    newConnections,

    -- ** DirectConnectGateway
    DirectConnectGateway (DirectConnectGateway'),
    newDirectConnectGateway,

    -- ** DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (DirectConnectGatewayAssociation'),
    newDirectConnectGatewayAssociation,

    -- ** DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (DirectConnectGatewayAssociationProposal'),
    newDirectConnectGatewayAssociationProposal,

    -- ** DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (DirectConnectGatewayAttachment'),
    newDirectConnectGatewayAttachment,

    -- ** Interconnect
    Interconnect (Interconnect'),
    newInterconnect,

    -- ** Lag
    Lag (Lag'),
    newLag,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** MacSecKey
    MacSecKey (MacSecKey'),
    newMacSecKey,

    -- ** NewBGPPeer
    NewBGPPeer (NewBGPPeer'),
    newNewBGPPeer,

    -- ** NewPrivateVirtualInterface
    NewPrivateVirtualInterface (NewPrivateVirtualInterface'),
    newNewPrivateVirtualInterface,

    -- ** NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (NewPrivateVirtualInterfaceAllocation'),
    newNewPrivateVirtualInterfaceAllocation,

    -- ** NewPublicVirtualInterface
    NewPublicVirtualInterface (NewPublicVirtualInterface'),
    newNewPublicVirtualInterface,

    -- ** NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (NewPublicVirtualInterfaceAllocation'),
    newNewPublicVirtualInterfaceAllocation,

    -- ** NewTransitVirtualInterface
    NewTransitVirtualInterface (NewTransitVirtualInterface'),
    newNewTransitVirtualInterface,

    -- ** NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (NewTransitVirtualInterfaceAllocation'),
    newNewTransitVirtualInterfaceAllocation,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** RouteFilterPrefix
    RouteFilterPrefix (RouteFilterPrefix'),
    newRouteFilterPrefix,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VirtualGateway
    VirtualGateway (VirtualGateway'),
    newVirtualGateway,

    -- ** VirtualInterface
    VirtualInterface (VirtualInterface'),
    newVirtualInterface,

    -- ** VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (VirtualInterfaceTestHistory'),
    newVirtualInterfaceTestHistory,
  )
where

import Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.AllocateHostedConnection
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.AllocateTransitVirtualInterface
import Network.AWS.DirectConnect.AssociateConnectionWithLag
import Network.AWS.DirectConnect.AssociateHostedConnection
import Network.AWS.DirectConnect.AssociateMacSecKey
import Network.AWS.DirectConnect.AssociateVirtualInterface
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
import Network.AWS.DirectConnect.CreateBGPPeer
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.CreateDirectConnectGateway
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.CreateInterconnect
import Network.AWS.DirectConnect.CreateLag
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.CreateTransitVirtualInterface
import Network.AWS.DirectConnect.DeleteBGPPeer
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.DeleteDirectConnectGateway
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.DeleteLag
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
import Network.AWS.DirectConnect.DescribeDirectConnectGateways
import Network.AWS.DirectConnect.DescribeHostedConnections
import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DescribeLags
import Network.AWS.DirectConnect.DescribeLoa
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.DescribeTags
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.DisassociateConnectionFromLag
import Network.AWS.DirectConnect.DisassociateMacSecKey
import Network.AWS.DirectConnect.Lens
import Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
import Network.AWS.DirectConnect.StartBgpFailoverTest
import Network.AWS.DirectConnect.StopBgpFailoverTest
import Network.AWS.DirectConnect.TagResource
import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.UntagResource
import Network.AWS.DirectConnect.UpdateConnection
import Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.UpdateLag
import Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
import Network.AWS.DirectConnect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DirectConnect'.

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
