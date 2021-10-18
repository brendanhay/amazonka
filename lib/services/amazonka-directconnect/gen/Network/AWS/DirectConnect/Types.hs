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
    _DirectConnectServerException,
    _TooManyTagsException,
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
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpStatus,
    bGPPeer_bgpPeerState,
    bGPPeer_amazonAddress,
    bGPPeer_addressFamily,
    bGPPeer_customerAddress,

    -- * Connection
    Connection (..),
    newConnection,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

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
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
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
    interconnect_interconnectId,
    interconnect_bandwidth,
    interconnect_awsDeviceV2,
    interconnect_providerName,
    interconnect_awsLogicalDeviceId,
    interconnect_hasLogicalRedundancy,
    interconnect_awsDevice,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_tags,
    interconnect_loaIssueTime,
    interconnect_region,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_location,

    -- * Lag
    Lag (..),
    newLag,
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,

    -- * Location
    Location (..),
    newLocation,
    location_availablePortSpeeds,
    location_availableMacSecPortSpeeds,
    location_availableProviders,
    location_locationCode,
    location_region,
    location_locationName,

    -- * MacSecKey
    MacSecKey (..),
    newMacSecKey,
    macSecKey_startOn,
    macSecKey_ckn,
    macSecKey_secretARN,
    macSecKey_state,

    -- * NewBGPPeer
    NewBGPPeer (..),
    newNewBGPPeer,
    newBGPPeer_authKey,
    newBGPPeer_asn,
    newBGPPeer_amazonAddress,
    newBGPPeer_addressFamily,
    newBGPPeer_customerAddress,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    newNewPrivateVirtualInterface,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_addressFamily,
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
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_addressFamily,
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
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_addressFamily,
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
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_addressFamily,
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
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_addressFamily,
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
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_addressFamily,
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
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
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

import qualified Network.AWS.Core as Core
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
import Network.AWS.DirectConnect.Types.MacSecKey
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DirectConnect",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "directconnect",
      Core._serviceSigningName = "directconnect",
      Core._serviceVersion = "2012-10-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DirectConnect",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | A server-side error occurred.
_DirectConnectServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectConnectServerException =
  Core._MatchServiceError
    defaultService
    "DirectConnectServerException"

-- | You have reached the limit on the number of tags that can be assigned.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeysException"

-- | One or more parameters are not valid.
_DirectConnectClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectConnectClientException =
  Core._MatchServiceError
    defaultService
    "DirectConnectClientException"
