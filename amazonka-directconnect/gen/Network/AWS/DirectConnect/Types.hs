{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TooManyTagsException,
    _DirectConnectServerException,
    _DuplicateTagKeysException,
    _DirectConnectClientException,

    -- * AddressFamily
    AddressFamily (..),

    -- * BGPPeerState
    BGPPeerState (..),

    -- * BGPStatus
    BGPStatus (..),

    -- * ConnectionState
    ConnectionState (..),

    -- * DirectConnectGatewayAssociationProposalState
    DirectConnectGatewayAssociationProposalState (..),

    -- * DirectConnectGatewayAssociationState
    DirectConnectGatewayAssociationState (..),

    -- * DirectConnectGatewayAttachmentState
    DirectConnectGatewayAttachmentState (..),

    -- * DirectConnectGatewayAttachmentType
    DirectConnectGatewayAttachmentType (..),

    -- * DirectConnectGatewayState
    DirectConnectGatewayState (..),

    -- * GatewayType
    GatewayType (..),

    -- * HasLogicalRedundancy
    HasLogicalRedundancy (..),

    -- * InterconnectState
    InterconnectState (..),

    -- * LagState
    LagState (..),

    -- * LoaContentType
    LoaContentType (..),

    -- * VirtualInterfaceState
    VirtualInterfaceState (..),

    -- * AssociatedGateway
    AssociatedGateway (..),
    newAssociatedGateway,
    associatedGateway_id,
    associatedGateway_type,
    associatedGateway_ownerAccount,
    associatedGateway_region,

    -- * BGPPeer
    BGPPeer (..),
    newBGPPeer,
    bGPPeer_authKey,
    bGPPeer_asn,
    bGPPeer_awsDeviceV2,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpStatus,
    bGPPeer_bgpPeerState,
    bGPPeer_addressFamily,
    bGPPeer_amazonAddress,
    bGPPeer_customerAddress,

    -- * Connection
    Connection (..),
    newConnection,
    connection_bandwidth,
    connection_connectionState,
    connection_awsDeviceV2,
    connection_connectionName,
    connection_providerName,
    connection_connectionId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_location,
    connection_vlan,

    -- * Connections
    Connections (..),
    newConnections,
    connections_connections,

    -- * DirectConnectGateway
    DirectConnectGateway (..),
    newDirectConnectGateway,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_stateChangeError,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_ownerAccount,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (..),
    newDirectConnectGatewayAssociation,
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_associationId,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,

    -- * DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (..),
    newDirectConnectGatewayAssociationProposal,
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalState,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (..),
    newDirectConnectGatewayAttachment,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_attachmentState,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceRegion,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_directConnectGatewayId,

    -- * Interconnect
    Interconnect (..),
    newInterconnect,
    interconnect_bandwidth,
    interconnect_interconnectId,
    interconnect_awsDeviceV2,
    interconnect_providerName,
    interconnect_hasLogicalRedundancy,
    interconnect_awsDevice,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_tags,
    interconnect_loaIssueTime,
    interconnect_region,
    interconnect_interconnectState,
    interconnect_location,
    interconnect_interconnectName,

    -- * Lag
    Lag (..),
    newLag,
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_providerName,
    lag_hasLogicalRedundancy,
    lag_connections,
    lag_awsDevice,
    lag_lagName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,

    -- * Location
    Location (..),
    newLocation,
    location_availablePortSpeeds,
    location_availableProviders,
    location_locationCode,
    location_region,
    location_locationName,

    -- * NewBGPPeer
    NewBGPPeer (..),
    newNewBGPPeer,
    newBGPPeer_authKey,
    newBGPPeer_asn,
    newBGPPeer_addressFamily,
    newBGPPeer_amazonAddress,
    newBGPPeer_customerAddress,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    newNewPrivateVirtualInterface,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- * NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (..),
    newNewPrivateVirtualInterfaceAllocation,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- * NewPublicVirtualInterface
    NewPublicVirtualInterface (..),
    newNewPublicVirtualInterface,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- * NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (..),
    newNewPublicVirtualInterfaceAllocation,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- * NewTransitVirtualInterface
    NewTransitVirtualInterface (..),
    newNewTransitVirtualInterface,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_virtualInterfaceName,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_vlan,
    newTransitVirtualInterface_customerAddress,

    -- * NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (..),
    newNewTransitVirtualInterfaceAllocation,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_vlan,
    newTransitVirtualInterfaceAllocation_customerAddress,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_resourceArn,
    resourceTag_tags,

    -- * RouteFilterPrefix
    RouteFilterPrefix (..),
    newRouteFilterPrefix,
    routeFilterPrefix_cidr,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * VirtualGateway
    VirtualGateway (..),
    newVirtualGateway,
    virtualGateway_virtualGatewayId,
    virtualGateway_virtualGatewayState,

    -- * VirtualInterface
    VirtualInterface (..),
    newVirtualInterface,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualInterfaceType,
    virtualInterface_mtu,
    virtualInterface_tags,
    virtualInterface_virtualInterfaceId,
    virtualInterface_amazonSideAsn,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceName,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_location,
    virtualInterface_vlan,
    virtualInterface_customerAddress,

    -- * VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (..),
    newVirtualInterfaceTestHistory,
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_startTime,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_virtualInterfaceId,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_testDurationInMinutes,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.BGPPeerState
import Network.AWS.DirectConnect.Types.BGPStatus
import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.Connections
import Network.AWS.DirectConnect.Types.DirectConnectGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
import Network.AWS.DirectConnect.Types.DirectConnectGatewayState
import Network.AWS.DirectConnect.Types.GatewayType
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.Interconnect
import Network.AWS.DirectConnect.Types.InterconnectState
import Network.AWS.DirectConnect.Types.Lag
import Network.AWS.DirectConnect.Types.LagState
import Network.AWS.DirectConnect.Types.LoaContentType
import Network.AWS.DirectConnect.Types.Location
import Network.AWS.DirectConnect.Types.NewBGPPeer
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.ResourceTag
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.VirtualGateway
import Network.AWS.DirectConnect.Types.VirtualInterface
import Network.AWS.DirectConnect.Types.VirtualInterfaceState
import Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-25@ of the Amazon Direct Connect SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "DirectConnect",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "directconnect",
      Prelude._svcVersion = "2012-10-25",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "DirectConnect",
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

-- | You have reached the limit on the number of tags that can be assigned.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | A server-side error occurred.
_DirectConnectServerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectConnectServerException =
  Prelude._MatchServiceError
    defaultService
    "DirectConnectServerException"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateTagKeysException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateTagKeysException"

-- | One or more parameters are not valid.
_DirectConnectClientException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectConnectClientException =
  Prelude._MatchServiceError
    defaultService
    "DirectConnectClientException"
