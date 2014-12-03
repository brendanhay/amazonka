{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.DirectConnect.Types
    (
    -- * Service
      DirectConnect
    -- ** Error
    , JSONError

    -- * VirtualInterface
    , VirtualInterface
    , virtualInterface
    , viAmazonAddress
    , viAsn
    , viAuthKey
    , viConnectionId
    , viCustomerAddress
    , viCustomerRouterConfig
    , viLocation
    , viOwnerAccount
    , viRouteFilterPrefixes
    , viVirtualGatewayId
    , viVirtualInterfaceId
    , viVirtualInterfaceName
    , viVirtualInterfaceState
    , viVirtualInterfaceType
    , viVlan

    -- * Location
    , Location
    , location
    , lLocationCode
    , lLocationName

    -- * Connections
    , Connections
    , connections
    , cConnections

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , newPrivateVirtualInterfaceAllocation
    , npviaAmazonAddress
    , npviaAsn
    , npviaAuthKey
    , npviaCustomerAddress
    , npviaVirtualInterfaceName
    , npviaVlan

    -- * VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- * Connection
    , Connection
    , connection
    , cBandwidth
    , cConnectionId
    , cConnectionName
    , cConnectionState
    , cLocation
    , cOwnerAccount
    , cPartnerName
    , cRegion
    , cVlan

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , newPublicVirtualInterface
    , npviAmazonAddress
    , npviAsn
    , npviAuthKey
    , npviCustomerAddress
    , npviRouteFilterPrefixes
    , npviVirtualInterfaceName
    , npviVlan

    -- * Interconnect
    , Interconnect
    , interconnect
    , iBandwidth
    , iInterconnectId
    , iInterconnectName
    , iInterconnectState
    , iLocation
    , iRegion

    -- * InterconnectState
    , InterconnectState (..)

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , newPrivateVirtualInterface
    , npvi1AmazonAddress
    , npvi1Asn
    , npvi1AuthKey
    , npvi1CustomerAddress
    , npvi1VirtualGatewayId
    , npvi1VirtualInterfaceName
    , npvi1Vlan

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , newPublicVirtualInterfaceAllocation
    , npvia1AmazonAddress
    , npvia1Asn
    , npvia1AuthKey
    , npvia1CustomerAddress
    , npvia1RouteFilterPrefixes
    , npvia1VirtualInterfaceName
    , npvia1Vlan

    -- * ConnectionState
    , ConnectionState (..)

    -- * VirtualGateway
    , VirtualGateway
    , virtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- * RouteFilterPrefix
    , RouteFilterPrefix
    , routeFilterPrefix
    , rfpCidr
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-10-25@ of the Amazon Direct Connect service.
data DirectConnect

instance AWSService DirectConnect where
    type Sg DirectConnect = V4
    type Er DirectConnect = JSONError

    service = service'
      where
        service' :: Service DirectConnect
        service' = Service
              { _svcAbbrev       = "DirectConnect"
              , _svcPrefix       = "directconnect"
              , _svcVersion      = "2012-10-25"
              , _svcTargetPrefix = Just "OvertureService"
              , _svcJSONVersion  = Just "1.1"
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry JSONError
        retry = Retry
            { _rPolicy   = exponentialBackon 0.05 2
            , _rAttempts = 5
            , _rCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && "Throttling" == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data VirtualInterface = VirtualInterface
    { _viAmazonAddress         :: Maybe Text
    , _viAsn                   :: Maybe Int
    , _viAuthKey               :: Maybe Text
    , _viConnectionId          :: Maybe Text
    , _viCustomerAddress       :: Maybe Text
    , _viCustomerRouterConfig  :: Maybe Text
    , _viLocation              :: Maybe Text
    , _viOwnerAccount          :: Maybe Text
    , _viRouteFilterPrefixes   :: List "routeFilterPrefixes" RouteFilterPrefix
    , _viVirtualGatewayId      :: Maybe Text
    , _viVirtualInterfaceId    :: Maybe Text
    , _viVirtualInterfaceName  :: Maybe Text
    , _viVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _viVirtualInterfaceType  :: Maybe Text
    , _viVlan                  :: Maybe Int
    } deriving (Eq, Show)

-- | 'VirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viAmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'viAsn' @::@ 'Maybe' 'Int'
--
-- * 'viAuthKey' @::@ 'Maybe' 'Text'
--
-- * 'viConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'viCustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'viCustomerRouterConfig' @::@ 'Maybe' 'Text'
--
-- * 'viLocation' @::@ 'Maybe' 'Text'
--
-- * 'viOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'viRouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'viVirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'viVirtualInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'viVirtualInterfaceName' @::@ 'Maybe' 'Text'
--
-- * 'viVirtualInterfaceState' @::@ 'Maybe' 'VirtualInterfaceState'
--
-- * 'viVirtualInterfaceType' @::@ 'Maybe' 'Text'
--
-- * 'viVlan' @::@ 'Maybe' 'Int'
--
virtualInterface :: VirtualInterface
virtualInterface = VirtualInterface
    { _viOwnerAccount          = Nothing
    , _viVirtualInterfaceId    = Nothing
    , _viLocation              = Nothing
    , _viConnectionId          = Nothing
    , _viVirtualInterfaceType  = Nothing
    , _viVirtualInterfaceName  = Nothing
    , _viVlan                  = Nothing
    , _viAsn                   = Nothing
    , _viAuthKey               = Nothing
    , _viAmazonAddress         = Nothing
    , _viCustomerAddress       = Nothing
    , _viVirtualInterfaceState = Nothing
    , _viCustomerRouterConfig  = Nothing
    , _viVirtualGatewayId      = Nothing
    , _viRouteFilterPrefixes   = mempty
    }

viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\s a -> s { _viAmazonAddress = a })

viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\s a -> s { _viAsn = a })

viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\s a -> s { _viAuthKey = a })

viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\s a -> s { _viConnectionId = a })

viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress =
    lens _viCustomerAddress (\s a -> s { _viCustomerAddress = a })

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig =
    lens _viCustomerRouterConfig (\s a -> s { _viCustomerRouterConfig = a })

viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\s a -> s { _viLocation = a })

viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\s a -> s { _viOwnerAccount = a })

viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes =
    lens _viRouteFilterPrefixes (\s a -> s { _viRouteFilterPrefixes = a })
        . _List

viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId =
    lens _viVirtualGatewayId (\s a -> s { _viVirtualGatewayId = a })

viVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceId =
    lens _viVirtualInterfaceId (\s a -> s { _viVirtualInterfaceId = a })

viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName =
    lens _viVirtualInterfaceName (\s a -> s { _viVirtualInterfaceName = a })

viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState =
    lens _viVirtualInterfaceState (\s a -> s { _viVirtualInterfaceState = a })

viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType =
    lens _viVirtualInterfaceType (\s a -> s { _viVirtualInterfaceType = a })

viVlan :: Lens' VirtualInterface (Maybe Int)
viVlan = lens _viVlan (\s a -> s { _viVlan = a })

instance FromJSON VirtualInterface where
    parseJSON = withObject "VirtualInterface" $ \o -> VirtualInterface
        <$> o .:? "amazonAddress"
        <*> o .:? "asn"
        <*> o .:? "authKey"
        <*> o .:? "connectionId"
        <*> o .:? "customerAddress"
        <*> o .:? "customerRouterConfig"
        <*> o .:? "location"
        <*> o .:? "ownerAccount"
        <*> o .:? "routeFilterPrefixes" .!= mempty
        <*> o .:? "virtualGatewayId"
        <*> o .:? "virtualInterfaceId"
        <*> o .:? "virtualInterfaceName"
        <*> o .:? "virtualInterfaceState"
        <*> o .:? "virtualInterfaceType"
        <*> o .:? "vlan"

instance ToJSON VirtualInterface where
    toJSON VirtualInterface{..} = object
        [ "ownerAccount"          .= _viOwnerAccount
        , "virtualInterfaceId"    .= _viVirtualInterfaceId
        , "location"              .= _viLocation
        , "connectionId"          .= _viConnectionId
        , "virtualInterfaceType"  .= _viVirtualInterfaceType
        , "virtualInterfaceName"  .= _viVirtualInterfaceName
        , "vlan"                  .= _viVlan
        , "asn"                   .= _viAsn
        , "authKey"               .= _viAuthKey
        , "amazonAddress"         .= _viAmazonAddress
        , "customerAddress"       .= _viCustomerAddress
        , "virtualInterfaceState" .= _viVirtualInterfaceState
        , "customerRouterConfig"  .= _viCustomerRouterConfig
        , "virtualGatewayId"      .= _viVirtualGatewayId
        , "routeFilterPrefixes"   .= _viRouteFilterPrefixes
        ]

data Location = Location
    { _lLocationCode :: Maybe Text
    , _lLocationName :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Location' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lLocationCode' @::@ 'Maybe' 'Text'
--
-- * 'lLocationName' @::@ 'Maybe' 'Text'
--
location :: Location
location = Location
    { _lLocationCode = Nothing
    , _lLocationName = Nothing
    }

-- | The code used to indicate the AWS Direct Connect location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\s a -> s { _lLocationCode = a })

-- | The name of the AWS Direct Connect location. The name includes the colocation
-- partner name and the physical site of the lit building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\s a -> s { _lLocationName = a })

instance FromJSON Location where
    parseJSON = withObject "Location" $ \o -> Location
        <$> o .:? "locationCode"
        <*> o .:? "locationName"

instance ToJSON Location where
    toJSON Location{..} = object
        [ "locationCode" .= _lLocationCode
        , "locationName" .= _lLocationName
        ]

newtype Connections = Connections
    { _cConnections :: List "connections" Connection
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList Connections where
    type Item Connections = Connection

    fromList = Connections . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _cConnections

-- | 'Connections' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cConnections' @::@ ['Connection']
--
connections :: Connections
connections = Connections
    { _cConnections = mempty
    }

-- | A list of connections.
cConnections :: Lens' Connections [Connection]
cConnections = lens _cConnections (\s a -> s { _cConnections = a }) . _List

instance FromJSON Connections where
    parseJSON = withObject "Connections" $ \o -> Connections
        <$> o .:? "connections" .!= mempty

instance ToJSON Connections where
    toJSON Connections{..} = object
        [ "connections" .= _cConnections
        ]

data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation
    { _npviaAmazonAddress        :: Maybe Text
    , _npviaAsn                  :: Int
    , _npviaAuthKey              :: Maybe Text
    , _npviaCustomerAddress      :: Maybe Text
    , _npviaVirtualInterfaceName :: Text
    , _npviaVlan                 :: Int
    } deriving (Eq, Ord, Show)

-- | 'NewPrivateVirtualInterfaceAllocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npviaAmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'npviaAsn' @::@ 'Int'
--
-- * 'npviaAuthKey' @::@ 'Maybe' 'Text'
--
-- * 'npviaCustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'npviaVirtualInterfaceName' @::@ 'Text'
--
-- * 'npviaVlan' @::@ 'Int'
--
newPrivateVirtualInterfaceAllocation :: Text -- ^ 'npviaVirtualInterfaceName'
                                     -> Int -- ^ 'npviaVlan'
                                     -> Int -- ^ 'npviaAsn'
                                     -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation p1 p2 p3 = NewPrivateVirtualInterfaceAllocation
    { _npviaVirtualInterfaceName = p1
    , _npviaVlan                 = p2
    , _npviaAsn                  = p3
    , _npviaAuthKey              = Nothing
    , _npviaAmazonAddress        = Nothing
    , _npviaCustomerAddress      = Nothing
    }

npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress =
    lens _npviaAmazonAddress (\s a -> s { _npviaAmazonAddress = a })

npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaAsn = lens _npviaAsn (\s a -> s { _npviaAsn = a })

npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\s a -> s { _npviaAuthKey = a })

npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress =
    lens _npviaCustomerAddress (\s a -> s { _npviaCustomerAddress = a })

npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName =
    lens _npviaVirtualInterfaceName
        (\s a -> s { _npviaVirtualInterfaceName = a })

npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\s a -> s { _npviaVlan = a })

instance FromJSON NewPrivateVirtualInterfaceAllocation where
    parseJSON = withObject "NewPrivateVirtualInterfaceAllocation" $ \o -> NewPrivateVirtualInterfaceAllocation
        <$> o .:? "amazonAddress"
        <*> o .:  "asn"
        <*> o .:? "authKey"
        <*> o .:? "customerAddress"
        <*> o .:  "virtualInterfaceName"
        <*> o .:  "vlan"

instance ToJSON NewPrivateVirtualInterfaceAllocation where
    toJSON NewPrivateVirtualInterfaceAllocation{..} = object
        [ "virtualInterfaceName" .= _npviaVirtualInterfaceName
        , "vlan"                 .= _npviaVlan
        , "asn"                  .= _npviaAsn
        , "authKey"              .= _npviaAuthKey
        , "amazonAddress"        .= _npviaAmazonAddress
        , "customerAddress"      .= _npviaCustomerAddress
        ]

data VirtualInterfaceState
    = Available  -- ^ available
    | Confirming -- ^ confirming
    | Deleted    -- ^ deleted
    | Deleting   -- ^ deleting
    | Pending    -- ^ pending
    | Rejected   -- ^ rejected
    | Verifying  -- ^ verifying
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VirtualInterfaceState

instance FromText VirtualInterfaceState where
    parser = takeText >>= \case
        "available"  -> pure Available
        "confirming" -> pure Confirming
        "deleted"    -> pure Deleted
        "deleting"   -> pure Deleting
        "pending"    -> pure Pending
        "rejected"   -> pure Rejected
        "verifying"  -> pure Verifying
        e            -> fail $
            "Failure parsing VirtualInterfaceState from " ++ show e

instance ToText VirtualInterfaceState where
    toText = \case
        Available  -> "available"
        Confirming -> "confirming"
        Deleted    -> "deleted"
        Deleting   -> "deleting"
        Pending    -> "pending"
        Rejected   -> "rejected"
        Verifying  -> "verifying"

instance ToByteString VirtualInterfaceState
instance ToHeader     VirtualInterfaceState
instance ToQuery      VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"

instance ToJSON VirtualInterfaceState where
    toJSON = toJSONText

data Connection = Connection
    { _cBandwidth       :: Maybe Text
    , _cConnectionId    :: Maybe Text
    , _cConnectionName  :: Maybe Text
    , _cConnectionState :: Maybe ConnectionState
    , _cLocation        :: Maybe Text
    , _cOwnerAccount    :: Maybe Text
    , _cPartnerName     :: Maybe Text
    , _cRegion          :: Maybe Text
    , _cVlan            :: Maybe Int
    } deriving (Eq, Show)

-- | 'Connection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'cConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'cConnectionName' @::@ 'Maybe' 'Text'
--
-- * 'cConnectionState' @::@ 'Maybe' 'ConnectionState'
--
-- * 'cLocation' @::@ 'Maybe' 'Text'
--
-- * 'cOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'cPartnerName' @::@ 'Maybe' 'Text'
--
-- * 'cRegion' @::@ 'Maybe' 'Text'
--
-- * 'cVlan' @::@ 'Maybe' 'Int'
--
connection :: Connection
connection = Connection
    { _cOwnerAccount    = Nothing
    , _cConnectionId    = Nothing
    , _cConnectionName  = Nothing
    , _cConnectionState = Nothing
    , _cRegion          = Nothing
    , _cLocation        = Nothing
    , _cBandwidth       = Nothing
    , _cVlan            = Nothing
    , _cPartnerName     = Nothing
    }

-- | Bandwidth of the connection.
--
-- Example: 1Gbps (for regular connections), or 500Mbps (for hosted connections)
--
-- Default: None
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\s a -> s { _cBandwidth = a })

cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\s a -> s { _cConnectionId = a })

cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\s a -> s { _cConnectionName = a })

cConnectionState :: Lens' Connection (Maybe ConnectionState)
cConnectionState = lens _cConnectionState (\s a -> s { _cConnectionState = a })

cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\s a -> s { _cLocation = a })

cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\s a -> s { _cOwnerAccount = a })

cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\s a -> s { _cPartnerName = a })

cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\s a -> s { _cRegion = a })

cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\s a -> s { _cVlan = a })

instance FromJSON Connection where
    parseJSON = withObject "Connection" $ \o -> Connection
        <$> o .:? "bandwidth"
        <*> o .:? "connectionId"
        <*> o .:? "connectionName"
        <*> o .:? "connectionState"
        <*> o .:? "location"
        <*> o .:? "ownerAccount"
        <*> o .:? "partnerName"
        <*> o .:? "region"
        <*> o .:? "vlan"

instance ToJSON Connection where
    toJSON Connection{..} = object
        [ "ownerAccount"    .= _cOwnerAccount
        , "connectionId"    .= _cConnectionId
        , "connectionName"  .= _cConnectionName
        , "connectionState" .= _cConnectionState
        , "region"          .= _cRegion
        , "location"        .= _cLocation
        , "bandwidth"       .= _cBandwidth
        , "vlan"            .= _cVlan
        , "partnerName"     .= _cPartnerName
        ]

data NewPublicVirtualInterface = NewPublicVirtualInterface
    { _npviAmazonAddress        :: Text
    , _npviAsn                  :: Int
    , _npviAuthKey              :: Maybe Text
    , _npviCustomerAddress      :: Text
    , _npviRouteFilterPrefixes  :: List "routeFilterPrefixes" RouteFilterPrefix
    , _npviVirtualInterfaceName :: Text
    , _npviVlan                 :: Int
    } deriving (Eq, Show)

-- | 'NewPublicVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npviAmazonAddress' @::@ 'Text'
--
-- * 'npviAsn' @::@ 'Int'
--
-- * 'npviAuthKey' @::@ 'Maybe' 'Text'
--
-- * 'npviCustomerAddress' @::@ 'Text'
--
-- * 'npviRouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'npviVirtualInterfaceName' @::@ 'Text'
--
-- * 'npviVlan' @::@ 'Int'
--
newPublicVirtualInterface :: Text -- ^ 'npviVirtualInterfaceName'
                          -> Int -- ^ 'npviVlan'
                          -> Int -- ^ 'npviAsn'
                          -> Text -- ^ 'npviAmazonAddress'
                          -> Text -- ^ 'npviCustomerAddress'
                          -> NewPublicVirtualInterface
newPublicVirtualInterface p1 p2 p3 p4 p5 = NewPublicVirtualInterface
    { _npviVirtualInterfaceName = p1
    , _npviVlan                 = p2
    , _npviAsn                  = p3
    , _npviAmazonAddress        = p4
    , _npviCustomerAddress      = p5
    , _npviAuthKey              = Nothing
    , _npviRouteFilterPrefixes  = mempty
    }

npviAmazonAddress :: Lens' NewPublicVirtualInterface Text
npviAmazonAddress =
    lens _npviAmazonAddress (\s a -> s { _npviAmazonAddress = a })

npviAsn :: Lens' NewPublicVirtualInterface Int
npviAsn = lens _npviAsn (\s a -> s { _npviAsn = a })

npviAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\s a -> s { _npviAuthKey = a })

npviCustomerAddress :: Lens' NewPublicVirtualInterface Text
npviCustomerAddress =
    lens _npviCustomerAddress (\s a -> s { _npviCustomerAddress = a })

npviRouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npviRouteFilterPrefixes =
    lens _npviRouteFilterPrefixes (\s a -> s { _npviRouteFilterPrefixes = a })
        . _List

npviVirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npviVirtualInterfaceName =
    lens _npviVirtualInterfaceName
        (\s a -> s { _npviVirtualInterfaceName = a })

npviVlan :: Lens' NewPublicVirtualInterface Int
npviVlan = lens _npviVlan (\s a -> s { _npviVlan = a })

instance FromJSON NewPublicVirtualInterface where
    parseJSON = withObject "NewPublicVirtualInterface" $ \o -> NewPublicVirtualInterface
        <$> o .:  "amazonAddress"
        <*> o .:  "asn"
        <*> o .:? "authKey"
        <*> o .:  "customerAddress"
        <*> o .:? "routeFilterPrefixes" .!= mempty
        <*> o .:  "virtualInterfaceName"
        <*> o .:  "vlan"

instance ToJSON NewPublicVirtualInterface where
    toJSON NewPublicVirtualInterface{..} = object
        [ "virtualInterfaceName" .= _npviVirtualInterfaceName
        , "vlan"                 .= _npviVlan
        , "asn"                  .= _npviAsn
        , "authKey"              .= _npviAuthKey
        , "amazonAddress"        .= _npviAmazonAddress
        , "customerAddress"      .= _npviCustomerAddress
        , "routeFilterPrefixes"  .= _npviRouteFilterPrefixes
        ]

data Interconnect = Interconnect
    { _iBandwidth         :: Maybe Text
    , _iInterconnectId    :: Maybe Text
    , _iInterconnectName  :: Maybe Text
    , _iInterconnectState :: Maybe InterconnectState
    , _iLocation          :: Maybe Text
    , _iRegion            :: Maybe Text
    } deriving (Eq, Show)

-- | 'Interconnect' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'iInterconnectId' @::@ 'Maybe' 'Text'
--
-- * 'iInterconnectName' @::@ 'Maybe' 'Text'
--
-- * 'iInterconnectState' @::@ 'Maybe' 'InterconnectState'
--
-- * 'iLocation' @::@ 'Maybe' 'Text'
--
-- * 'iRegion' @::@ 'Maybe' 'Text'
--
interconnect :: Interconnect
interconnect = Interconnect
    { _iInterconnectId    = Nothing
    , _iInterconnectName  = Nothing
    , _iInterconnectState = Nothing
    , _iRegion            = Nothing
    , _iLocation          = Nothing
    , _iBandwidth         = Nothing
    }

iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\s a -> s { _iBandwidth = a })

iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\s a -> s { _iInterconnectId = a })

iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName =
    lens _iInterconnectName (\s a -> s { _iInterconnectName = a })

iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState =
    lens _iInterconnectState (\s a -> s { _iInterconnectState = a })

iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\s a -> s { _iLocation = a })

iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\s a -> s { _iRegion = a })

instance FromJSON Interconnect where
    parseJSON = withObject "Interconnect" $ \o -> Interconnect
        <$> o .:? "bandwidth"
        <*> o .:? "interconnectId"
        <*> o .:? "interconnectName"
        <*> o .:? "interconnectState"
        <*> o .:? "location"
        <*> o .:? "region"

instance ToJSON Interconnect where
    toJSON Interconnect{..} = object
        [ "interconnectId"    .= _iInterconnectId
        , "interconnectName"  .= _iInterconnectName
        , "interconnectState" .= _iInterconnectState
        , "region"            .= _iRegion
        , "location"          .= _iLocation
        , "bandwidth"         .= _iBandwidth
        ]

data InterconnectState
    = ISAvailable -- ^ available
    | ISDeleted   -- ^ deleted
    | ISDeleting  -- ^ deleting
    | ISDown      -- ^ down
    | ISPending   -- ^ pending
    | ISRequested -- ^ requested
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InterconnectState

instance FromText InterconnectState where
    parser = takeText >>= \case
        "available" -> pure ISAvailable
        "deleted"   -> pure ISDeleted
        "deleting"  -> pure ISDeleting
        "down"      -> pure ISDown
        "pending"   -> pure ISPending
        "requested" -> pure ISRequested
        e           -> fail $
            "Failure parsing InterconnectState from " ++ show e

instance ToText InterconnectState where
    toText = \case
        ISAvailable -> "available"
        ISDeleted   -> "deleted"
        ISDeleting  -> "deleting"
        ISDown      -> "down"
        ISPending   -> "pending"
        ISRequested -> "requested"

instance ToByteString InterconnectState
instance ToHeader     InterconnectState
instance ToQuery      InterconnectState

instance FromJSON InterconnectState where
    parseJSON = parseJSONText "InterconnectState"

instance ToJSON InterconnectState where
    toJSON = toJSONText

data NewPrivateVirtualInterface = NewPrivateVirtualInterface
    { _npvi1AmazonAddress        :: Maybe Text
    , _npvi1Asn                  :: Int
    , _npvi1AuthKey              :: Maybe Text
    , _npvi1CustomerAddress      :: Maybe Text
    , _npvi1VirtualGatewayId     :: Text
    , _npvi1VirtualInterfaceName :: Text
    , _npvi1Vlan                 :: Int
    } deriving (Eq, Ord, Show)

-- | 'NewPrivateVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npvi1AmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'npvi1Asn' @::@ 'Int'
--
-- * 'npvi1AuthKey' @::@ 'Maybe' 'Text'
--
-- * 'npvi1CustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'npvi1VirtualGatewayId' @::@ 'Text'
--
-- * 'npvi1VirtualInterfaceName' @::@ 'Text'
--
-- * 'npvi1Vlan' @::@ 'Int'
--
newPrivateVirtualInterface :: Text -- ^ 'npvi1VirtualInterfaceName'
                           -> Int -- ^ 'npvi1Vlan'
                           -> Int -- ^ 'npvi1Asn'
                           -> Text -- ^ 'npvi1VirtualGatewayId'
                           -> NewPrivateVirtualInterface
newPrivateVirtualInterface p1 p2 p3 p4 = NewPrivateVirtualInterface
    { _npvi1VirtualInterfaceName = p1
    , _npvi1Vlan                 = p2
    , _npvi1Asn                  = p3
    , _npvi1VirtualGatewayId     = p4
    , _npvi1AuthKey              = Nothing
    , _npvi1AmazonAddress        = Nothing
    , _npvi1CustomerAddress      = Nothing
    }

npvi1AmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npvi1AmazonAddress =
    lens _npvi1AmazonAddress (\s a -> s { _npvi1AmazonAddress = a })

npvi1Asn :: Lens' NewPrivateVirtualInterface Int
npvi1Asn = lens _npvi1Asn (\s a -> s { _npvi1Asn = a })

npvi1AuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
npvi1AuthKey = lens _npvi1AuthKey (\s a -> s { _npvi1AuthKey = a })

npvi1CustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npvi1CustomerAddress =
    lens _npvi1CustomerAddress (\s a -> s { _npvi1CustomerAddress = a })

npvi1VirtualGatewayId :: Lens' NewPrivateVirtualInterface Text
npvi1VirtualGatewayId =
    lens _npvi1VirtualGatewayId (\s a -> s { _npvi1VirtualGatewayId = a })

npvi1VirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
npvi1VirtualInterfaceName =
    lens _npvi1VirtualInterfaceName
        (\s a -> s { _npvi1VirtualInterfaceName = a })

npvi1Vlan :: Lens' NewPrivateVirtualInterface Int
npvi1Vlan = lens _npvi1Vlan (\s a -> s { _npvi1Vlan = a })

instance FromJSON NewPrivateVirtualInterface where
    parseJSON = withObject "NewPrivateVirtualInterface" $ \o -> NewPrivateVirtualInterface
        <$> o .:? "amazonAddress"
        <*> o .:  "asn"
        <*> o .:? "authKey"
        <*> o .:? "customerAddress"
        <*> o .:  "virtualGatewayId"
        <*> o .:  "virtualInterfaceName"
        <*> o .:  "vlan"

instance ToJSON NewPrivateVirtualInterface where
    toJSON NewPrivateVirtualInterface{..} = object
        [ "virtualInterfaceName" .= _npvi1VirtualInterfaceName
        , "vlan"                 .= _npvi1Vlan
        , "asn"                  .= _npvi1Asn
        , "authKey"              .= _npvi1AuthKey
        , "amazonAddress"        .= _npvi1AmazonAddress
        , "customerAddress"      .= _npvi1CustomerAddress
        , "virtualGatewayId"     .= _npvi1VirtualGatewayId
        ]

data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation
    { _npvia1AmazonAddress        :: Text
    , _npvia1Asn                  :: Int
    , _npvia1AuthKey              :: Maybe Text
    , _npvia1CustomerAddress      :: Text
    , _npvia1RouteFilterPrefixes  :: List "routeFilterPrefixes" RouteFilterPrefix
    , _npvia1VirtualInterfaceName :: Text
    , _npvia1Vlan                 :: Int
    } deriving (Eq, Show)

-- | 'NewPublicVirtualInterfaceAllocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npvia1AmazonAddress' @::@ 'Text'
--
-- * 'npvia1Asn' @::@ 'Int'
--
-- * 'npvia1AuthKey' @::@ 'Maybe' 'Text'
--
-- * 'npvia1CustomerAddress' @::@ 'Text'
--
-- * 'npvia1RouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'npvia1VirtualInterfaceName' @::@ 'Text'
--
-- * 'npvia1Vlan' @::@ 'Int'
--
newPublicVirtualInterfaceAllocation :: Text -- ^ 'npvia1VirtualInterfaceName'
                                    -> Int -- ^ 'npvia1Vlan'
                                    -> Int -- ^ 'npvia1Asn'
                                    -> Text -- ^ 'npvia1AmazonAddress'
                                    -> Text -- ^ 'npvia1CustomerAddress'
                                    -> NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation p1 p2 p3 p4 p5 = NewPublicVirtualInterfaceAllocation
    { _npvia1VirtualInterfaceName = p1
    , _npvia1Vlan                 = p2
    , _npvia1Asn                  = p3
    , _npvia1AmazonAddress        = p4
    , _npvia1CustomerAddress      = p5
    , _npvia1AuthKey              = Nothing
    , _npvia1RouteFilterPrefixes  = mempty
    }

npvia1AmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1AmazonAddress =
    lens _npvia1AmazonAddress (\s a -> s { _npvia1AmazonAddress = a })

npvia1Asn :: Lens' NewPublicVirtualInterfaceAllocation Int
npvia1Asn = lens _npvia1Asn (\s a -> s { _npvia1Asn = a })

npvia1AuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
npvia1AuthKey = lens _npvia1AuthKey (\s a -> s { _npvia1AuthKey = a })

npvia1CustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1CustomerAddress =
    lens _npvia1CustomerAddress (\s a -> s { _npvia1CustomerAddress = a })

npvia1RouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
npvia1RouteFilterPrefixes =
    lens _npvia1RouteFilterPrefixes
        (\s a -> s { _npvia1RouteFilterPrefixes = a })
            . _List

npvia1VirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1VirtualInterfaceName =
    lens _npvia1VirtualInterfaceName
        (\s a -> s { _npvia1VirtualInterfaceName = a })

npvia1Vlan :: Lens' NewPublicVirtualInterfaceAllocation Int
npvia1Vlan = lens _npvia1Vlan (\s a -> s { _npvia1Vlan = a })

instance FromJSON NewPublicVirtualInterfaceAllocation where
    parseJSON = withObject "NewPublicVirtualInterfaceAllocation" $ \o -> NewPublicVirtualInterfaceAllocation
        <$> o .:  "amazonAddress"
        <*> o .:  "asn"
        <*> o .:? "authKey"
        <*> o .:  "customerAddress"
        <*> o .:? "routeFilterPrefixes" .!= mempty
        <*> o .:  "virtualInterfaceName"
        <*> o .:  "vlan"

instance ToJSON NewPublicVirtualInterfaceAllocation where
    toJSON NewPublicVirtualInterfaceAllocation{..} = object
        [ "virtualInterfaceName" .= _npvia1VirtualInterfaceName
        , "vlan"                 .= _npvia1Vlan
        , "asn"                  .= _npvia1Asn
        , "authKey"              .= _npvia1AuthKey
        , "amazonAddress"        .= _npvia1AmazonAddress
        , "customerAddress"      .= _npvia1CustomerAddress
        , "routeFilterPrefixes"  .= _npvia1RouteFilterPrefixes
        ]

data ConnectionState
    = CSAvailable -- ^ available
    | CSDeleted   -- ^ deleted
    | CSDeleting  -- ^ deleting
    | CSDown      -- ^ down
    | CSOrdering  -- ^ ordering
    | CSPending   -- ^ pending
    | CSRejected  -- ^ rejected
    | CSRequested -- ^ requested
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ConnectionState

instance FromText ConnectionState where
    parser = takeText >>= \case
        "available" -> pure CSAvailable
        "deleted"   -> pure CSDeleted
        "deleting"  -> pure CSDeleting
        "down"      -> pure CSDown
        "ordering"  -> pure CSOrdering
        "pending"   -> pure CSPending
        "rejected"  -> pure CSRejected
        "requested" -> pure CSRequested
        e           -> fail $
            "Failure parsing ConnectionState from " ++ show e

instance ToText ConnectionState where
    toText = \case
        CSAvailable -> "available"
        CSDeleted   -> "deleted"
        CSDeleting  -> "deleting"
        CSDown      -> "down"
        CSOrdering  -> "ordering"
        CSPending   -> "pending"
        CSRejected  -> "rejected"
        CSRequested -> "requested"

instance ToByteString ConnectionState
instance ToHeader     ConnectionState
instance ToQuery      ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"

instance ToJSON ConnectionState where
    toJSON = toJSONText

data VirtualGateway = VirtualGateway
    { _vgVirtualGatewayId    :: Maybe Text
    , _vgVirtualGatewayState :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'VirtualGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vgVirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'vgVirtualGatewayState' @::@ 'Maybe' 'Text'
--
virtualGateway :: VirtualGateway
virtualGateway = VirtualGateway
    { _vgVirtualGatewayId    = Nothing
    , _vgVirtualGatewayState = Nothing
    }

vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId =
    lens _vgVirtualGatewayId (\s a -> s { _vgVirtualGatewayId = a })

vgVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayState =
    lens _vgVirtualGatewayState (\s a -> s { _vgVirtualGatewayState = a })

instance FromJSON VirtualGateway where
    parseJSON = withObject "VirtualGateway" $ \o -> VirtualGateway
        <$> o .:? "virtualGatewayId"
        <*> o .:? "virtualGatewayState"

instance ToJSON VirtualGateway where
    toJSON VirtualGateway{..} = object
        [ "virtualGatewayId"    .= _vgVirtualGatewayId
        , "virtualGatewayState" .= _vgVirtualGatewayState
        ]

newtype RouteFilterPrefix = RouteFilterPrefix
    { _rfpCidr :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'RouteFilterPrefix' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfpCidr' @::@ 'Maybe' 'Text'
--
routeFilterPrefix :: RouteFilterPrefix
routeFilterPrefix = RouteFilterPrefix
    { _rfpCidr = Nothing
    }

-- | CIDR notation for the advertised route. Multiple routes are separated by
-- commas.
--
-- Example: 10.10.10.0/24,10.10.11.0/24
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\s a -> s { _rfpCidr = a })

instance FromJSON RouteFilterPrefix where
    parseJSON = withObject "RouteFilterPrefix" $ \o -> RouteFilterPrefix
        <$> o .:? "cidr"

instance ToJSON RouteFilterPrefix where
    toJSON RouteFilterPrefix{..} = object
        [ "cidr" .= _rfpCidr
        ]
