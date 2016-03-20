{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types
    (
    -- * Service Configuration
      directConnect

    -- * Errors
    , _DirectConnectClientException
    , _DirectConnectServerException

    -- * ConnectionState
    , ConnectionState (..)

    -- * InterconnectState
    , InterconnectState (..)

    -- * VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- * Connection
    , Connection
    , connection
    , cVlan
    , cLocation
    , cConnectionId
    , cPartnerName
    , cConnectionName
    , cBandwidth
    , cOwnerAccount
    , cRegion
    , cConnectionState

    -- * Connections
    , Connections
    , connections
    , cConnections

    -- * Interconnect
    , Interconnect
    , interconnect
    , iInterconnectId
    , iLocation
    , iInterconnectName
    , iBandwidth
    , iInterconnectState
    , iRegion

    -- * Location
    , Location
    , location
    , lLocationName
    , lLocationCode

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , newPrivateVirtualInterface
    , nCustomerAddress
    , nAmazonAddress
    , nAuthKey
    , nVirtualInterfaceName
    , nVlan
    , nAsn
    , nVirtualGatewayId

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , newPrivateVirtualInterfaceAllocation
    , npviaCustomerAddress
    , npviaAmazonAddress
    , npviaAuthKey
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , newPublicVirtualInterface
    , npviAuthKey
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAmazonAddress
    , npviCustomerAddress
    , npviRouteFilterPrefixes

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , newPublicVirtualInterfaceAllocation
    , newAuthKey
    , newVirtualInterfaceName
    , newVlan
    , newAsn
    , newAmazonAddress
    , newCustomerAddress
    , newRouteFilterPrefixes

    -- * RouteFilterPrefix
    , RouteFilterPrefix
    , routeFilterPrefix
    , rfpCidr

    -- * VirtualGateway
    , VirtualGateway
    , virtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- * VirtualInterface
    , VirtualInterface
    , virtualInterface
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viVirtualInterfaceState
    , viConnectionId
    , viVirtualInterfaceType
    , viAsn
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.DirectConnect.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2012-10-25' of the Amazon Direct Connect SDK configuration.
directConnect :: Service
directConnect =
    Service
    { _svcAbbrev = "DirectConnect"
    , _svcSigner = v4
    , _svcPrefix = "directconnect"
    , _svcVersion = "2012-10-25"
    , _svcEndpoint = defaultEndpoint directConnect
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The API was called with invalid parameters. The error message will
-- contain additional details about the cause.
_DirectConnectClientException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectConnectClientException =
    _ServiceError . hasCode "DirectConnectClientException"

-- | A server-side error occurred during the API call. The error message will
-- contain additional details about the cause.
_DirectConnectServerException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectConnectServerException =
    _ServiceError . hasCode "DirectConnectServerException"
