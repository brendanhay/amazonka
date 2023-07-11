{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DirectConnectClientException,
    _DirectConnectServerException,
    _DuplicateTagKeysException,
    _TooManyTagsException,

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

    -- * NniPartnerType
    NniPartnerType (..),

    -- * VirtualInterfaceState
    VirtualInterfaceState (..),

    -- * AssociatedGateway
    AssociatedGateway (..),
    newAssociatedGateway,
    associatedGateway_id,
    associatedGateway_ownerAccount,
    associatedGateway_region,
    associatedGateway_type,

    -- * BGPPeer
    BGPPeer (..),
    newBGPPeer,
    bGPPeer_addressFamily,
    bGPPeer_amazonAddress,
    bGPPeer_asn,
    bGPPeer_authKey,
    bGPPeer_awsDeviceV2,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpPeerState,
    bGPPeer_bgpStatus,
    bGPPeer_customerAddress,

    -- * Connection
    Connection (..),
    newConnection,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- * Connections
    Connections (..),
    newConnections,
    connections_connections,

    -- * CustomerAgreement
    CustomerAgreement (..),
    newCustomerAgreement,
    customerAgreement_agreementName,
    customerAgreement_status,

    -- * DirectConnectGateway
    DirectConnectGateway (..),
    newDirectConnectGateway,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_ownerAccount,
    directConnectGateway_stateChangeError,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (..),
    newDirectConnectGatewayAssociation,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_associationId,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_virtualGatewayRegion,

    -- * DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (..),
    newDirectConnectGatewayAssociationProposal,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_proposalState,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (..),
    newDirectConnectGatewayAttachment,
    directConnectGatewayAttachment_attachmentState,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_directConnectGatewayId,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceRegion,

    -- * Interconnect
    Interconnect (..),
    newInterconnect,
    interconnect_awsDevice,
    interconnect_awsDeviceV2,
    interconnect_awsLogicalDeviceId,
    interconnect_bandwidth,
    interconnect_hasLogicalRedundancy,
    interconnect_interconnectId,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_loaIssueTime,
    interconnect_location,
    interconnect_providerName,
    interconnect_region,
    interconnect_tags,

    -- * Lag
    Lag (..),
    newLag,
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,

    -- * Location
    Location (..),
    newLocation,
    location_availableMacSecPortSpeeds,
    location_availablePortSpeeds,
    location_availableProviders,
    location_locationCode,
    location_locationName,
    location_region,

    -- * MacSecKey
    MacSecKey (..),
    newMacSecKey,
    macSecKey_ckn,
    macSecKey_secretARN,
    macSecKey_startOn,
    macSecKey_state,

    -- * NewBGPPeer
    NewBGPPeer (..),
    newNewBGPPeer,
    newBGPPeer_addressFamily,
    newBGPPeer_amazonAddress,
    newBGPPeer_asn,
    newBGPPeer_authKey,
    newBGPPeer_customerAddress,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    newNewPrivateVirtualInterface,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_enableSiteLink,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- * NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (..),
    newNewPrivateVirtualInterfaceAllocation,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- * NewPublicVirtualInterface
    NewPublicVirtualInterface (..),
    newNewPublicVirtualInterface,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- * NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (..),
    newNewPublicVirtualInterfaceAllocation,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- * NewTransitVirtualInterface
    NewTransitVirtualInterface (..),
    newNewTransitVirtualInterface,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_customerAddress,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_enableSiteLink,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_virtualInterfaceName,
    newTransitVirtualInterface_vlan,

    -- * NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (..),
    newNewTransitVirtualInterfaceAllocation,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_customerAddress,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,
    newTransitVirtualInterfaceAllocation_vlan,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_resourceArn,
    resourceTag_tags,

    -- * RouteFilterPrefix
    RouteFilterPrefix (..),
    newRouteFilterPrefix,
    routeFilterPrefix_cidr,

    -- * RouterType
    RouterType (..),
    newRouterType,
    routerType_platform,
    routerType_routerTypeIdentifier,
    routerType_software,
    routerType_vendor,
    routerType_xsltTemplateName,
    routerType_xsltTemplateNameForMacSec,

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
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- * VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (..),
    newVirtualInterfaceTestHistory,
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_startTime,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_testDurationInMinutes,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_virtualInterfaceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types.AddressFamily
import Amazonka.DirectConnect.Types.AssociatedGateway
import Amazonka.DirectConnect.Types.BGPPeer
import Amazonka.DirectConnect.Types.BGPPeerState
import Amazonka.DirectConnect.Types.BGPStatus
import Amazonka.DirectConnect.Types.Connection
import Amazonka.DirectConnect.Types.ConnectionState
import Amazonka.DirectConnect.Types.Connections
import Amazonka.DirectConnect.Types.CustomerAgreement
import Amazonka.DirectConnect.Types.DirectConnectGateway
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociation
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationState
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachment
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentType
import Amazonka.DirectConnect.Types.DirectConnectGatewayState
import Amazonka.DirectConnect.Types.GatewayType
import Amazonka.DirectConnect.Types.HasLogicalRedundancy
import Amazonka.DirectConnect.Types.Interconnect
import Amazonka.DirectConnect.Types.InterconnectState
import Amazonka.DirectConnect.Types.Lag
import Amazonka.DirectConnect.Types.LagState
import Amazonka.DirectConnect.Types.LoaContentType
import Amazonka.DirectConnect.Types.Location
import Amazonka.DirectConnect.Types.MacSecKey
import Amazonka.DirectConnect.Types.NewBGPPeer
import Amazonka.DirectConnect.Types.NewPrivateVirtualInterface
import Amazonka.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.NewPublicVirtualInterface
import Amazonka.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.NewTransitVirtualInterface
import Amazonka.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.NniPartnerType
import Amazonka.DirectConnect.Types.ResourceTag
import Amazonka.DirectConnect.Types.RouteFilterPrefix
import Amazonka.DirectConnect.Types.RouterType
import Amazonka.DirectConnect.Types.Tag
import Amazonka.DirectConnect.Types.VirtualGateway
import Amazonka.DirectConnect.Types.VirtualInterface
import Amazonka.DirectConnect.Types.VirtualInterfaceState
import Amazonka.DirectConnect.Types.VirtualInterfaceTestHistory
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-10-25@ of the Amazon Direct Connect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DirectConnect",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "directconnect",
      Core.signingName = "directconnect",
      Core.version = "2012-10-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DirectConnect",
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

-- | One or more parameters are not valid.
_DirectConnectClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DirectConnectClientException =
  Core._MatchServiceError
    defaultService
    "DirectConnectClientException"

-- | A server-side error occurred.
_DirectConnectServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DirectConnectServerException =
  Core._MatchServiceError
    defaultService
    "DirectConnectServerException"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeysException"

-- | You have reached the limit on the number of tags that can be assigned.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
