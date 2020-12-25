-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _DirectConnectClientException,
    _DuplicateTagKeysException,
    _TooManyTagsException,
    _DirectConnectServerException,

    -- * VirtualInterface
    VirtualInterface (..),
    mkVirtualInterface,
    viAddressFamily,
    viAmazonAddress,
    viAmazonSideAsn,
    viAsn,
    viAuthKey,
    viAwsDeviceV2,
    viBgpPeers,
    viConnectionId,
    viCustomerAddress,
    viCustomerRouterConfig,
    viDirectConnectGatewayId,
    viJumboFrameCapable,
    viLocation,
    viMtu,
    viOwnerAccount,
    viRegion,
    viRouteFilterPrefixes,
    viTags,
    viVirtualGatewayId,
    viVirtualInterfaceId,
    viVirtualInterfaceName,
    viVirtualInterfaceState,
    viVirtualInterfaceType,
    viVlan,

    -- * VirtualGatewayId
    VirtualGatewayId (..),

    -- * PaginationToken
    PaginationToken (..),

    -- * NewTransitVirtualInterface
    NewTransitVirtualInterface (..),
    mkNewTransitVirtualInterface,
    ntviAddressFamily,
    ntviAmazonAddress,
    ntviAsn,
    ntviAuthKey,
    ntviCustomerAddress,
    ntviDirectConnectGatewayId,
    ntviMtu,
    ntviTags,
    ntviVirtualInterfaceName,
    ntviVlan,

    -- * LagId
    LagId (..),

    -- * BGPPeer
    BGPPeer (..),
    mkBGPPeer,
    bgppAddressFamily,
    bgppAmazonAddress,
    bgppAsn,
    bgppAuthKey,
    bgppAwsDeviceV2,
    bgppBgpPeerId,
    bgppBgpPeerState,
    bgppBgpStatus,
    bgppCustomerAddress,

    -- * InterconnectId
    InterconnectId (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (..),
    mkDirectConnectGatewayAssociation,
    dcgaAllowedPrefixesToDirectConnectGateway,
    dcgaAssociatedGateway,
    dcgaAssociationId,
    dcgaAssociationState,
    dcgaDirectConnectGatewayId,
    dcgaDirectConnectGatewayOwnerAccount,
    dcgaStateChangeError,
    dcgaVirtualGatewayId,
    dcgaVirtualGatewayOwnerAccount,
    dcgaVirtualGatewayRegion,

    -- * BGPAuthKey
    BGPAuthKey (..),

    -- * CustomerAddress
    CustomerAddress (..),

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (..),
    mkDirectConnectGatewayAttachment,
    dAttachmentState,
    dAttachmentType,
    dDirectConnectGatewayId,
    dStateChangeError,
    dVirtualInterfaceId,
    dVirtualInterfaceOwnerAccount,
    dVirtualInterfaceRegion,

    -- * AwsDevice
    AwsDevice (..),

    -- * Location
    Location (..),
    mkLocation,
    lAvailablePortSpeeds,
    lAvailableProviders,
    lLocationCode,
    lLocationName,
    lRegion,

    -- * LagName
    LagName (..),

    -- * InterconnectName
    InterconnectName (..),

    -- * Connections
    Connections (..),
    mkConnections,
    cConnections,

    -- * AssociatedGatewayId
    AssociatedGatewayId (..),

    -- * DirectConnectGatewayAttachmentState
    DirectConnectGatewayAttachmentState (..),

    -- * NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (..),
    mkNewPrivateVirtualInterfaceAllocation,
    npviaVirtualInterfaceName,
    npviaVlan,
    npviaAsn,
    npviaAddressFamily,
    npviaAmazonAddress,
    npviaAuthKey,
    npviaCustomerAddress,
    npviaMtu,
    npviaTags,

    -- * DirectConnectGatewayAssociationState
    DirectConnectGatewayAssociationState (..),

    -- * HasLogicalRedundancy
    HasLogicalRedundancy (..),

    -- * AddressFamily
    AddressFamily (..),

    -- * AmazonAddress
    AmazonAddress (..),

    -- * FailureTestHistoryStatus
    FailureTestHistoryStatus (..),

    -- * GatewayIdentifier
    GatewayIdentifier (..),

    -- * VirtualInterfaceState
    VirtualInterfaceState (..),

    -- * DirectConnectGatewayAssociationProposalId
    DirectConnectGatewayAssociationProposalId (..),

    -- * TestId
    TestId (..),

    -- * DirectConnectGateway
    DirectConnectGateway (..),
    mkDirectConnectGateway,
    dcgAmazonSideAsn,
    dcgDirectConnectGatewayId,
    dcgDirectConnectGatewayName,
    dcgDirectConnectGatewayState,
    dcgOwnerAccount,
    dcgStateChangeError,

    -- * ConnectionId
    ConnectionId (..),

    -- * DirectConnectGatewayId
    DirectConnectGatewayId (..),

    -- * DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (..),
    mkDirectConnectGatewayAssociationProposal,
    dcgapAssociatedGateway,
    dcgapDirectConnectGatewayId,
    dcgapDirectConnectGatewayOwnerAccount,
    dcgapExistingAllowedPrefixesToDirectConnectGateway,
    dcgapProposalId,
    dcgapProposalState,
    dcgapRequestedAllowedPrefixesToDirectConnectGateway,

    -- * PortSpeed
    PortSpeed (..),

    -- * Connection
    Connection (..),
    mkConnection,
    cAwsDevice,
    cAwsDeviceV2,
    cBandwidth,
    cConnectionId,
    cConnectionName,
    cConnectionState,
    cHasLogicalRedundancy,
    cJumboFrameCapable,
    cLagId,
    cLoaIssueTime,
    cLocation,
    cOwnerAccount,
    cPartnerName,
    cProviderName,
    cRegion,
    cTags,
    cVlan,

    -- * NewPublicVirtualInterface
    NewPublicVirtualInterface (..),
    mkNewPublicVirtualInterface,
    npviVirtualInterfaceName,
    npviVlan,
    npviAsn,
    npviAddressFamily,
    npviAmazonAddress,
    npviAuthKey,
    npviCustomerAddress,
    npviRouteFilterPrefixes,
    npviTags,

    -- * StateChangeError
    StateChangeError (..),

    -- * PartnerName
    PartnerName (..),

    -- * BGPStatus
    BGPStatus (..),

    -- * VirtualGatewayRegion
    VirtualGatewayRegion (..),

    -- * ConnectionName
    ConnectionName (..),

    -- * DirectConnectGatewayName
    DirectConnectGatewayName (..),

    -- * VirtualInterfaceType
    VirtualInterfaceType (..),

    -- * CIDR
    CIDR (..),

    -- * VirtualInterfaceRegion
    VirtualInterfaceRegion (..),

    -- * DirectConnectGatewayState
    DirectConnectGatewayState (..),

    -- * DirectConnectGatewayAssociationId
    DirectConnectGatewayAssociationId (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (..),
    mkNewTransitVirtualInterfaceAllocation,
    ntviaAddressFamily,
    ntviaAmazonAddress,
    ntviaAsn,
    ntviaAuthKey,
    ntviaCustomerAddress,
    ntviaMtu,
    ntviaTags,
    ntviaVirtualInterfaceName,
    ntviaVlan,

    -- * Bandwidth
    Bandwidth (..),

    -- * LocationName
    LocationName (..),

    -- * Lag
    Lag (..),
    mkLag,
    lfAllowsHostedConnections,
    lfAwsDevice,
    lfAwsDeviceV2,
    lfConnections,
    lfConnectionsBandwidth,
    lfHasLogicalRedundancy,
    lfJumboFrameCapable,
    lfLagId,
    lfLagName,
    lfLagState,
    lfLocation,
    lfMinimumLinks,
    lfNumberOfConnections,
    lfOwnerAccount,
    lfProviderName,
    lfRegion,
    lfTags,

    -- * DirectConnectGatewayAttachmentType
    DirectConnectGatewayAttachmentType (..),

    -- * Interconnect
    Interconnect (..),
    mkInterconnect,
    iAwsDevice,
    iAwsDeviceV2,
    iBandwidth,
    iHasLogicalRedundancy,
    iInterconnectId,
    iInterconnectName,
    iInterconnectState,
    iJumboFrameCapable,
    iLagId,
    iLoaIssueTime,
    iLocation,
    iProviderName,
    iRegion,
    iTags,

    -- * BGPPeerId
    BGPPeerId (..),

    -- * GatewayIdToAssociate
    GatewayIdToAssociate (..),

    -- * InterconnectState
    InterconnectState (..),

    -- * LagState
    LagState (..),

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    mkNewPrivateVirtualInterface,
    nVirtualInterfaceName,
    nVlan,
    nAsn,
    nAddressFamily,
    nAmazonAddress,
    nAuthKey,
    nCustomerAddress,
    nDirectConnectGatewayId,
    nMtu,
    nTags,
    nVirtualGatewayId,

    -- * TagKey
    TagKey (..),

    -- * Region
    Region (..),

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtResourceArn,
    rtTags,

    -- * LocationCode
    LocationCode (..),

    -- * VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (..),
    mkVirtualInterfaceTestHistory,
    vithBgpPeers,
    vithEndTime,
    vithOwnerAccount,
    vithStartTime,
    vithStatus,
    vithTestDurationInMinutes,
    vithTestId,
    vithVirtualInterfaceId,

    -- * OwnerAccount
    OwnerAccount (..),

    -- * AssociatedGateway
    AssociatedGateway (..),
    mkAssociatedGateway,
    agId,
    agOwnerAccount,
    agRegion,
    agType,

    -- * BGPPeerState
    BGPPeerState (..),

    -- * GatewayType
    GatewayType (..),

    -- * LoaContentType
    LoaContentType (..),

    -- * NewBGPPeer
    NewBGPPeer (..),
    mkNewBGPPeer,
    nbgppAddressFamily,
    nbgppAmazonAddress,
    nbgppAsn,
    nbgppAuthKey,
    nbgppCustomerAddress,

    -- * VirtualInterfaceName
    VirtualInterfaceName (..),

    -- * VirtualGatewayState
    VirtualGatewayState (..),

    -- * ProviderName
    ProviderName (..),

    -- * NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (..),
    mkNewPublicVirtualInterfaceAllocation,
    npviafVirtualInterfaceName,
    npviafVlan,
    npviafAsn,
    npviafAddressFamily,
    npviafAmazonAddress,
    npviafAuthKey,
    npviafCustomerAddress,
    npviafRouteFilterPrefixes,
    npviafTags,

    -- * ConnectionState
    ConnectionState (..),

    -- * VirtualGateway
    VirtualGateway (..),
    mkVirtualGateway,
    vgVirtualGatewayId,
    vgVirtualGatewayState,

    -- * VirtualInterfaceId
    VirtualInterfaceId (..),

    -- * AwsDeviceV2
    AwsDeviceV2 (..),

    -- * RouteFilterPrefix
    RouteFilterPrefix (..),
    mkRouteFilterPrefix,
    rfpCidr,

    -- * DirectConnectGatewayAssociationProposalState
    DirectConnectGatewayAssociationProposalState (..),

    -- * NextToken
    NextToken (..),

    -- * AuthKey
    AuthKey (..),

    -- * CustomerRouterConfig
    CustomerRouterConfig (..),

    -- * ProposalId
    ProposalId (..),

    -- * AssociatedGatewayOwnerAccount
    AssociatedGatewayOwnerAccount (..),

    -- * BgpPeerId
    BgpPeerId (..),

    -- * GatewayId
    GatewayId (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * AssociationId
    AssociationId (..),

    -- * DirectConnectGatewayOwnerAccount
    DirectConnectGatewayOwnerAccount (..),

    -- * VirtualGatewayOwnerAccount
    VirtualGatewayOwnerAccount (..),

    -- * VirtualInterfaceOwnerAccount
    VirtualInterfaceOwnerAccount (..),
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.AmazonAddress
import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.AssociatedGatewayId
import Network.AWS.DirectConnect.Types.AssociatedGatewayOwnerAccount
import Network.AWS.DirectConnect.Types.AssociationId
import Network.AWS.DirectConnect.Types.AuthKey
import Network.AWS.DirectConnect.Types.AwsDevice
import Network.AWS.DirectConnect.Types.AwsDeviceV2
import Network.AWS.DirectConnect.Types.BGPAuthKey
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.BGPPeerId
import Network.AWS.DirectConnect.Types.BGPPeerState
import Network.AWS.DirectConnect.Types.BGPStatus
import Network.AWS.DirectConnect.Types.Bandwidth
import Network.AWS.DirectConnect.Types.BgpPeerId
import Network.AWS.DirectConnect.Types.CIDR
import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.ConnectionId
import Network.AWS.DirectConnect.Types.ConnectionName
import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.Connections
import Network.AWS.DirectConnect.Types.CustomerAddress
import Network.AWS.DirectConnect.Types.CustomerRouterConfig
import Network.AWS.DirectConnect.Types.DirectConnectGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationId
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalId
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
import Network.AWS.DirectConnect.Types.DirectConnectGatewayId
import Network.AWS.DirectConnect.Types.DirectConnectGatewayName
import Network.AWS.DirectConnect.Types.DirectConnectGatewayOwnerAccount
import Network.AWS.DirectConnect.Types.DirectConnectGatewayState
import Network.AWS.DirectConnect.Types.FailureTestHistoryStatus
import Network.AWS.DirectConnect.Types.GatewayId
import Network.AWS.DirectConnect.Types.GatewayIdToAssociate
import Network.AWS.DirectConnect.Types.GatewayIdentifier
import Network.AWS.DirectConnect.Types.GatewayType
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.Interconnect
import Network.AWS.DirectConnect.Types.InterconnectId
import Network.AWS.DirectConnect.Types.InterconnectName
import Network.AWS.DirectConnect.Types.InterconnectState
import Network.AWS.DirectConnect.Types.Key
import Network.AWS.DirectConnect.Types.Lag
import Network.AWS.DirectConnect.Types.LagId
import Network.AWS.DirectConnect.Types.LagName
import Network.AWS.DirectConnect.Types.LagState
import Network.AWS.DirectConnect.Types.LoaContentType
import Network.AWS.DirectConnect.Types.Location
import Network.AWS.DirectConnect.Types.LocationCode
import Network.AWS.DirectConnect.Types.LocationName
import Network.AWS.DirectConnect.Types.NewBGPPeer
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NextToken
import Network.AWS.DirectConnect.Types.OwnerAccount
import Network.AWS.DirectConnect.Types.PaginationToken
import Network.AWS.DirectConnect.Types.PartnerName
import Network.AWS.DirectConnect.Types.PortSpeed
import Network.AWS.DirectConnect.Types.ProposalId
import Network.AWS.DirectConnect.Types.ProviderName
import Network.AWS.DirectConnect.Types.Region
import Network.AWS.DirectConnect.Types.ResourceArn
import Network.AWS.DirectConnect.Types.ResourceTag
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.StateChangeError
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.TagKey
import Network.AWS.DirectConnect.Types.TestId
import Network.AWS.DirectConnect.Types.Value
import Network.AWS.DirectConnect.Types.VirtualGateway
import Network.AWS.DirectConnect.Types.VirtualGatewayId
import Network.AWS.DirectConnect.Types.VirtualGatewayOwnerAccount
import Network.AWS.DirectConnect.Types.VirtualGatewayRegion
import Network.AWS.DirectConnect.Types.VirtualGatewayState
import Network.AWS.DirectConnect.Types.VirtualInterface
import Network.AWS.DirectConnect.Types.VirtualInterfaceId
import Network.AWS.DirectConnect.Types.VirtualInterfaceName
import Network.AWS.DirectConnect.Types.VirtualInterfaceOwnerAccount
import Network.AWS.DirectConnect.Types.VirtualInterfaceRegion
import Network.AWS.DirectConnect.Types.VirtualInterfaceState
import Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
import Network.AWS.DirectConnect.Types.VirtualInterfaceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-25@ of the Amazon Direct Connect SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "DirectConnect",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "directconnect",
      Core._svcVersion = "2012-10-25",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "DirectConnect",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | One or more parameters are not valid.
_DirectConnectClientException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectConnectClientException =
  Core._MatchServiceError
    mkServiceConfig
    "DirectConnectClientException"
{-# DEPRECATED _DirectConnectClientException "Use generic-lens or generic-optics instead." #-}

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateTagKeysException"
{-# DEPRECATED _DuplicateTagKeysException "Use generic-lens or generic-optics instead." #-}

-- | You have reached the limit on the number of tags that can be assigned.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead." #-}

-- | A server-side error occurred.
_DirectConnectServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectConnectServerException =
  Core._MatchServiceError
    mkServiceConfig
    "DirectConnectServerException"
{-# DEPRECATED _DirectConnectServerException "Use generic-lens or generic-optics instead." #-}
