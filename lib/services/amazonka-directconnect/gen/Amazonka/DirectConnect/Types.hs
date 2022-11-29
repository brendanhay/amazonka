{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DirectConnectServerException,
    _DuplicateTagKeysException,
    _TooManyTagsException,
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

    -- * NniPartnerType
    NniPartnerType (..),

    -- * VirtualInterfaceState
    VirtualInterfaceState (..),

    -- * AssociatedGateway
    AssociatedGateway (..),
    newAssociatedGateway,
    associatedGateway_type,
    associatedGateway_id,
    associatedGateway_region,
    associatedGateway_ownerAccount,

    -- * BGPPeer
    BGPPeer (..),
    newBGPPeer,
    bGPPeer_bgpPeerId,
    bGPPeer_addressFamily,
    bGPPeer_authKey,
    bGPPeer_bgpPeerState,
    bGPPeer_customerAddress,
    bGPPeer_asn,
    bGPPeer_amazonAddress,
    bGPPeer_bgpStatus,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_awsDeviceV2,

    -- * Connection
    Connection (..),
    newConnection,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

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
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_stateChangeError,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_ownerAccount,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (..),
    newDirectConnectGatewayAssociation,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_associationId,

    -- * DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (..),
    newDirectConnectGatewayAssociationProposal,
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalState,

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (..),
    newDirectConnectGatewayAttachment,
    directConnectGatewayAttachment_directConnectGatewayId,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_virtualInterfaceRegion,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_attachmentState,

    -- * Interconnect
    Interconnect (..),
    newInterconnect,
    interconnect_tags,
    interconnect_providerName,
    interconnect_bandwidth,
    interconnect_interconnectName,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_hasLogicalRedundancy,
    interconnect_loaIssueTime,
    interconnect_interconnectState,
    interconnect_awsDevice,
    interconnect_location,
    interconnect_region,
    interconnect_interconnectId,
    interconnect_awsLogicalDeviceId,
    interconnect_awsDeviceV2,

    -- * Lag
    Lag (..),
    newLag,
    lag_tags,
    lag_numberOfConnections,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_macSecCapable,
    lag_providerName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_hasLogicalRedundancy,
    lag_awsDevice,
    lag_lagName,
    lag_location,
    lag_region,
    lag_allowsHostedConnections,
    lag_connections,
    lag_connectionsBandwidth,
    lag_ownerAccount,
    lag_awsLogicalDeviceId,
    lag_encryptionMode,
    lag_awsDeviceV2,

    -- * Location
    Location (..),
    newLocation,
    location_availablePortSpeeds,
    location_region,
    location_availableProviders,
    location_locationName,
    location_locationCode,
    location_availableMacSecPortSpeeds,

    -- * MacSecKey
    MacSecKey (..),
    newMacSecKey,
    macSecKey_startOn,
    macSecKey_state,
    macSecKey_secretARN,
    macSecKey_ckn,

    -- * NewBGPPeer
    NewBGPPeer (..),
    newNewBGPPeer,
    newBGPPeer_addressFamily,
    newBGPPeer_authKey,
    newBGPPeer_customerAddress,
    newBGPPeer_asn,
    newBGPPeer_amazonAddress,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    newNewPrivateVirtualInterface,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_enableSiteLink,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- * NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (..),
    newNewPrivateVirtualInterfaceAllocation,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- * NewPublicVirtualInterface
    NewPublicVirtualInterface (..),
    newNewPublicVirtualInterface,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- * NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (..),
    newNewPublicVirtualInterfaceAllocation,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- * NewTransitVirtualInterface
    NewTransitVirtualInterface (..),
    newNewTransitVirtualInterface,
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_vlan,
    newTransitVirtualInterface_customerAddress,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_enableSiteLink,
    newTransitVirtualInterface_virtualInterfaceName,

    -- * NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (..),
    newNewTransitVirtualInterfaceAllocation,
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_vlan,
    newTransitVirtualInterfaceAllocation_customerAddress,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_tags,
    resourceTag_resourceArn,

    -- * RouteFilterPrefix
    RouteFilterPrefix (..),
    newRouteFilterPrefix,
    routeFilterPrefix_cidr,

    -- * RouterType
    RouterType (..),
    newRouterType,
    routerType_xsltTemplateName,
    routerType_routerTypeIdentifier,
    routerType_software,
    routerType_platform,
    routerType_xsltTemplateNameForMacSec,
    routerType_vendor,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * VirtualGateway
    VirtualGateway (..),
    newVirtualGateway,
    virtualGateway_virtualGatewayState,
    virtualGateway_virtualGatewayId,

    -- * VirtualInterface
    VirtualInterface (..),
    newVirtualInterface,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_siteLinkEnabled,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- * VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (..),
    newVirtualInterfaceTestHistory,
    virtualInterfaceTestHistory_testDurationInMinutes,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_virtualInterfaceId,
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_startTime,
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
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A server-side error occurred.
_DirectConnectServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectConnectServerException =
  Core._MatchServiceError
    defaultService
    "DirectConnectServerException"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeysException"

-- | You have reached the limit on the number of tags that can be assigned.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | One or more parameters are not valid.
_DirectConnectClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectConnectClientException =
  Core._MatchServiceError
    defaultService
    "DirectConnectClientException"
