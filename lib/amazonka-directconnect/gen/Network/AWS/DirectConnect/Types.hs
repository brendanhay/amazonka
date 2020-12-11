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
    directConnectService,

    -- * Errors

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
    mkAssociatedGateway,
    agId,
    agOwnerAccount,
    agRegion,
    agType,

    -- * BGPPeer
    BGPPeer (..),
    mkBGPPeer,
    bpCustomerAddress,
    bpAmazonAddress,
    bpAddressFamily,
    bpBgpStatus,
    bpAsn,
    bpAuthKey,
    bpBgpPeerId,
    bpBgpPeerState,
    bpAwsDeviceV2,

    -- * Connection
    Connection (..),
    mkConnection,
    cLagId,
    cVlan,
    cLocation,
    cAwsDevice,
    cHasLogicalRedundancy,
    cConnectionId,
    cLoaIssueTime,
    cPartnerName,
    cConnectionName,
    cBandwidth,
    cJumboFrameCapable,
    cOwnerAccount,
    cRegion,
    cProviderName,
    cAwsDeviceV2,
    cConnectionState,
    cTags,

    -- * Connections
    Connections (..),
    mkConnections,
    cConnections,

    -- * DirectConnectGateway
    DirectConnectGateway (..),
    mkDirectConnectGateway,
    dcgDirectConnectGatewayId,
    dcgStateChangeError,
    dcgAmazonSideASN,
    dcgDirectConnectGatewayName,
    dcgDirectConnectGatewayState,
    dcgOwnerAccount,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation (..),
    mkDirectConnectGatewayAssociation,
    dcgaVirtualGatewayId,
    dcgaAssociationId,
    dcgaDirectConnectGatewayId,
    dcgaVirtualGatewayOwnerAccount,
    dcgaStateChangeError,
    dcgaVirtualGatewayRegion,
    dcgaAssociatedGateway,
    dcgaDirectConnectGatewayOwnerAccount,
    dcgaAllowedPrefixesToDirectConnectGateway,
    dcgaAssociationState,

    -- * DirectConnectGatewayAssociationProposal
    DirectConnectGatewayAssociationProposal (..),
    mkDirectConnectGatewayAssociationProposal,
    dcgapExistingAllowedPrefixesToDirectConnectGateway,
    dcgapDirectConnectGatewayId,
    dcgapProposalId,
    dcgapAssociatedGateway,
    dcgapProposalState,
    dcgapDirectConnectGatewayOwnerAccount,
    dcgapRequestedAllowedPrefixesToDirectConnectGateway,

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment (..),
    mkDirectConnectGatewayAttachment,
    dDirectConnectGatewayId,
    dAttachmentState,
    dStateChangeError,
    dVirtualInterfaceRegion,
    dVirtualInterfaceOwnerAccount,
    dVirtualInterfaceId,
    dAttachmentType,

    -- * Interconnect
    Interconnect (..),
    mkInterconnect,
    iLagId,
    iInterconnectId,
    iLocation,
    iInterconnectName,
    iAwsDevice,
    iHasLogicalRedundancy,
    iLoaIssueTime,
    iBandwidth,
    iJumboFrameCapable,
    iInterconnectState,
    iRegion,
    iProviderName,
    iAwsDeviceV2,
    iTags,

    -- * Lag
    Lag (..),
    mkLag,
    lagLagId,
    lagConnectionsBandwidth,
    lagMinimumLinks,
    lagLagName,
    lagLocation,
    lagConnections,
    lagAwsDevice,
    lagHasLogicalRedundancy,
    lagAllowsHostedConnections,
    lagNumberOfConnections,
    lagJumboFrameCapable,
    lagLagState,
    lagOwnerAccount,
    lagRegion,
    lagProviderName,
    lagAwsDeviceV2,
    lagTags,

    -- * Location
    Location (..),
    mkLocation,
    lAvailablePortSpeeds,
    lLocationName,
    lLocationCode,
    lRegion,
    lAvailableProviders,

    -- * NewBGPPeer
    NewBGPPeer (..),
    mkNewBGPPeer,
    nbpCustomerAddress,
    nbpAmazonAddress,
    nbpAddressFamily,
    nbpAsn,
    nbpAuthKey,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface (..),
    mkNewPrivateVirtualInterface,
    nVirtualGatewayId,
    nMtu,
    nCustomerAddress,
    nAmazonAddress,
    nAddressFamily,
    nDirectConnectGatewayId,
    nAuthKey,
    nTags,
    nVirtualInterfaceName,
    nVlan,
    nAsn,

    -- * NewPrivateVirtualInterfaceAllocation
    NewPrivateVirtualInterfaceAllocation (..),
    mkNewPrivateVirtualInterfaceAllocation,
    npviaMtu,
    npviaCustomerAddress,
    npviaAmazonAddress,
    npviaAddressFamily,
    npviaAuthKey,
    npviaTags,
    npviaVirtualInterfaceName,
    npviaVlan,
    npviaAsn,

    -- * NewPublicVirtualInterface
    NewPublicVirtualInterface (..),
    mkNewPublicVirtualInterface,
    npviRouteFilterPrefixes,
    npviCustomerAddress,
    npviAmazonAddress,
    npviAddressFamily,
    npviAuthKey,
    npviTags,
    npviVirtualInterfaceName,
    npviVlan,
    npviAsn,

    -- * NewPublicVirtualInterfaceAllocation
    NewPublicVirtualInterfaceAllocation (..),
    mkNewPublicVirtualInterfaceAllocation,
    newRouteFilterPrefixes,
    newCustomerAddress,
    newAmazonAddress,
    newAddressFamily,
    newAuthKey,
    newTags,
    newVirtualInterfaceName,
    newVlan,
    newAsn,

    -- * NewTransitVirtualInterface
    NewTransitVirtualInterface (..),
    mkNewTransitVirtualInterface,
    ntviMtu,
    ntviCustomerAddress,
    ntviVlan,
    ntviAmazonAddress,
    ntviAddressFamily,
    ntviDirectConnectGatewayId,
    ntviAsn,
    ntviAuthKey,
    ntviVirtualInterfaceName,
    ntviTags,

    -- * NewTransitVirtualInterfaceAllocation
    NewTransitVirtualInterfaceAllocation (..),
    mkNewTransitVirtualInterfaceAllocation,
    ntviaMtu,
    ntviaCustomerAddress,
    ntviaVlan,
    ntviaAmazonAddress,
    ntviaAddressFamily,
    ntviaAsn,
    ntviaAuthKey,
    ntviaVirtualInterfaceName,
    ntviaTags,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtResourceARN,
    rtTags,

    -- * RouteFilterPrefix
    RouteFilterPrefix (..),
    mkRouteFilterPrefix,
    rfpCidr,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * VirtualGateway
    VirtualGateway (..),
    mkVirtualGateway,
    vgVirtualGatewayId,
    vgVirtualGatewayState,

    -- * VirtualInterface
    VirtualInterface (..),
    mkVirtualInterface,
    viBgpPeers,
    viVirtualGatewayId,
    viMtu,
    viRouteFilterPrefixes,
    viCustomerAddress,
    viVlan,
    viLocation,
    viAmazonAddress,
    viAddressFamily,
    viVirtualInterfaceState,
    viConnectionId,
    viDirectConnectGatewayId,
    viAmazonSideASN,
    viVirtualInterfaceType,
    viAsn,
    viAuthKey,
    viJumboFrameCapable,
    viCustomerRouterConfig,
    viOwnerAccount,
    viRegion,
    viVirtualInterfaceName,
    viAwsDeviceV2,
    viVirtualInterfaceId,
    viTags,

    -- * VirtualInterfaceTestHistory
    VirtualInterfaceTestHistory (..),
    mkVirtualInterfaceTestHistory,
    vithBgpPeers,
    vithStatus,
    vithTestDurationInMinutes,
    vithStartTime,
    vithTestId,
    vithEndTime,
    vithOwnerAccount,
    vithVirtualInterfaceId,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-25@ of the Amazon Direct Connect SDK configuration.
directConnectService :: Lude.Service
directConnectService =
  Lude.Service
    { Lude._svcAbbrev = "DirectConnect",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "directconnect",
      Lude._svcVersion = "2012-10-25",
      Lude._svcEndpoint = Lude.defaultEndpoint directConnectService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DirectConnect",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
