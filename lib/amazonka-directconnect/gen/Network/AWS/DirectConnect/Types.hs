{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types
  ( -- * Service Configuration
    directConnect,

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
    AssociatedGateway,
    associatedGateway,
    agId,
    agOwnerAccount,
    agRegion,
    agType,

    -- * BGPPeer
    BGPPeer,
    bgpPeer,
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
    Connection,
    connection,
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
    Connections,
    connections,
    cConnections,

    -- * DirectConnectGateway
    DirectConnectGateway,
    directConnectGateway,
    dcgDirectConnectGatewayId,
    dcgStateChangeError,
    dcgAmazonSideASN,
    dcgDirectConnectGatewayName,
    dcgDirectConnectGatewayState,
    dcgOwnerAccount,

    -- * DirectConnectGatewayAssociation
    DirectConnectGatewayAssociation,
    directConnectGatewayAssociation,
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
    DirectConnectGatewayAssociationProposal,
    directConnectGatewayAssociationProposal,
    dcgapExistingAllowedPrefixesToDirectConnectGateway,
    dcgapDirectConnectGatewayId,
    dcgapProposalId,
    dcgapAssociatedGateway,
    dcgapProposalState,
    dcgapDirectConnectGatewayOwnerAccount,
    dcgapRequestedAllowedPrefixesToDirectConnectGateway,

    -- * DirectConnectGatewayAttachment
    DirectConnectGatewayAttachment,
    directConnectGatewayAttachment,
    dDirectConnectGatewayId,
    dAttachmentState,
    dStateChangeError,
    dVirtualInterfaceRegion,
    dVirtualInterfaceOwnerAccount,
    dVirtualInterfaceId,
    dAttachmentType,

    -- * Interconnect
    Interconnect,
    interconnect,
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
    Lag,
    lag,
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
    Location,
    location,
    lAvailablePortSpeeds,
    lLocationName,
    lLocationCode,
    lRegion,
    lAvailableProviders,

    -- * NewBGPPeer
    NewBGPPeer,
    newBGPPeer,
    nbpCustomerAddress,
    nbpAmazonAddress,
    nbpAddressFamily,
    nbpAsn,
    nbpAuthKey,

    -- * NewPrivateVirtualInterface
    NewPrivateVirtualInterface,
    newPrivateVirtualInterface,
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
    NewPrivateVirtualInterfaceAllocation,
    newPrivateVirtualInterfaceAllocation,
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
    NewPublicVirtualInterface,
    newPublicVirtualInterface,
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
    NewPublicVirtualInterfaceAllocation,
    newPublicVirtualInterfaceAllocation,
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
    NewTransitVirtualInterface,
    newTransitVirtualInterface,
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
    NewTransitVirtualInterfaceAllocation,
    newTransitVirtualInterfaceAllocation,
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
    ResourceTag,
    resourceTag,
    rtResourceARN,
    rtTags,

    -- * RouteFilterPrefix
    RouteFilterPrefix,
    routeFilterPrefix,
    rfpCidr,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * VirtualGateway
    VirtualGateway,
    virtualGateway,
    vgVirtualGatewayId,
    vgVirtualGatewayState,

    -- * VirtualInterface
    VirtualInterface,
    virtualInterface,
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
    VirtualInterfaceTestHistory,
    virtualInterfaceTestHistory,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-10-25@ of the Amazon Direct Connect SDK configuration.
directConnect :: Service
directConnect =
  Service
    { _svcAbbrev = "DirectConnect",
      _svcSigner = v4,
      _svcPrefix = "directconnect",
      _svcVersion = "2012-10-25",
      _svcEndpoint = defaultEndpoint directConnect,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DirectConnect",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
