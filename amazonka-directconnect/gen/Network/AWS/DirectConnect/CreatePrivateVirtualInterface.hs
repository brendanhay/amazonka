{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePrivateVirtualInterface.html>
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    (
    -- * Request
      CreatePrivateVirtualInterface
    -- ** Request constructor
    , createPrivateVirtualInterface
    -- ** Request lenses
    , cpvi1ConnectionId
    , cpvi1NewPrivateVirtualInterface

    -- * Response
    , CreatePrivateVirtualInterfaceResponse
    -- ** Response constructor
    , createPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvir2AmazonAddress
    , cpvir2Asn
    , cpvir2AuthKey
    , cpvir2ConnectionId
    , cpvir2CustomerAddress
    , cpvir2CustomerRouterConfig
    , cpvir2Location
    , cpvir2OwnerAccount
    , cpvir2RouteFilterPrefixes
    , cpvir2VirtualGatewayId
    , cpvir2VirtualInterfaceId
    , cpvir2VirtualInterfaceName
    , cpvir2VirtualInterfaceState
    , cpvir2VirtualInterfaceType
    , cpvir2Vlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { _cpvi1ConnectionId               :: Text
    , _cpvi1NewPrivateVirtualInterface :: NewPrivateVirtualInterface
    } deriving (Eq, Show)

-- | 'CreatePrivateVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvi1ConnectionId' @::@ 'Text'
--
-- * 'cpvi1NewPrivateVirtualInterface' @::@ 'NewPrivateVirtualInterface'
--
createPrivateVirtualInterface :: Text -- ^ 'cpvi1ConnectionId'
                              -> NewPrivateVirtualInterface -- ^ 'cpvi1NewPrivateVirtualInterface'
                              -> CreatePrivateVirtualInterface
createPrivateVirtualInterface p1 p2 = CreatePrivateVirtualInterface
    { _cpvi1ConnectionId               = p1
    , _cpvi1NewPrivateVirtualInterface = p2
    }

cpvi1ConnectionId :: Lens' CreatePrivateVirtualInterface Text
cpvi1ConnectionId =
    lens _cpvi1ConnectionId (\s a -> s { _cpvi1ConnectionId = a })

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
cpvi1NewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
cpvi1NewPrivateVirtualInterface =
    lens _cpvi1NewPrivateVirtualInterface
        (\s a -> s { _cpvi1NewPrivateVirtualInterface = a })

data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _cpvir2AmazonAddress         :: Maybe Text
    , _cpvir2Asn                   :: Maybe Int
    , _cpvir2AuthKey               :: Maybe Text
    , _cpvir2ConnectionId          :: Maybe Text
    , _cpvir2CustomerAddress       :: Maybe Text
    , _cpvir2CustomerRouterConfig  :: Maybe Text
    , _cpvir2Location              :: Maybe Text
    , _cpvir2OwnerAccount          :: Maybe Text
    , _cpvir2RouteFilterPrefixes   :: List "routeFilterPrefixes" RouteFilterPrefix
    , _cpvir2VirtualGatewayId      :: Maybe Text
    , _cpvir2VirtualInterfaceId    :: Maybe Text
    , _cpvir2VirtualInterfaceName  :: Maybe Text
    , _cpvir2VirtualInterfaceState :: Maybe Text
    , _cpvir2VirtualInterfaceType  :: Maybe Text
    , _cpvir2Vlan                  :: Maybe Int
    } deriving (Eq, Show)

-- | 'CreatePrivateVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvir2AmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2Asn' @::@ 'Maybe' 'Int'
--
-- * 'cpvir2AuthKey' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2ConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2CustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2CustomerRouterConfig' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2Location' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2OwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2RouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'cpvir2VirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2VirtualInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2VirtualInterfaceName' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2VirtualInterfaceState' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2VirtualInterfaceType' @::@ 'Maybe' 'Text'
--
-- * 'cpvir2Vlan' @::@ 'Maybe' 'Int'
--
createPrivateVirtualInterfaceResponse :: CreatePrivateVirtualInterfaceResponse
createPrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _cpvir2OwnerAccount          = Nothing
    , _cpvir2VirtualInterfaceId    = Nothing
    , _cpvir2Location              = Nothing
    , _cpvir2ConnectionId          = Nothing
    , _cpvir2VirtualInterfaceType  = Nothing
    , _cpvir2VirtualInterfaceName  = Nothing
    , _cpvir2Vlan                  = Nothing
    , _cpvir2Asn                   = Nothing
    , _cpvir2AuthKey               = Nothing
    , _cpvir2AmazonAddress         = Nothing
    , _cpvir2CustomerAddress       = Nothing
    , _cpvir2VirtualInterfaceState = Nothing
    , _cpvir2CustomerRouterConfig  = Nothing
    , _cpvir2VirtualGatewayId      = Nothing
    , _cpvir2RouteFilterPrefixes   = mempty
    }

cpvir2AmazonAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2AmazonAddress =
    lens _cpvir2AmazonAddress (\s a -> s { _cpvir2AmazonAddress = a })

cpvir2Asn :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Int)
cpvir2Asn = lens _cpvir2Asn (\s a -> s { _cpvir2Asn = a })

cpvir2AuthKey :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2AuthKey = lens _cpvir2AuthKey (\s a -> s { _cpvir2AuthKey = a })

cpvir2ConnectionId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2ConnectionId =
    lens _cpvir2ConnectionId (\s a -> s { _cpvir2ConnectionId = a })

cpvir2CustomerAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2CustomerAddress =
    lens _cpvir2CustomerAddress (\s a -> s { _cpvir2CustomerAddress = a })

-- | Information for generating the customer router configuration.
cpvir2CustomerRouterConfig :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2CustomerRouterConfig =
    lens _cpvir2CustomerRouterConfig
        (\s a -> s { _cpvir2CustomerRouterConfig = a })

cpvir2Location :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2Location = lens _cpvir2Location (\s a -> s { _cpvir2Location = a })

cpvir2OwnerAccount :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2OwnerAccount =
    lens _cpvir2OwnerAccount (\s a -> s { _cpvir2OwnerAccount = a })

cpvir2RouteFilterPrefixes :: Lens' CreatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
cpvir2RouteFilterPrefixes =
    lens _cpvir2RouteFilterPrefixes
        (\s a -> s { _cpvir2RouteFilterPrefixes = a })
            . _List

cpvir2VirtualGatewayId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2VirtualGatewayId =
    lens _cpvir2VirtualGatewayId (\s a -> s { _cpvir2VirtualGatewayId = a })

cpvir2VirtualInterfaceId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2VirtualInterfaceId =
    lens _cpvir2VirtualInterfaceId
        (\s a -> s { _cpvir2VirtualInterfaceId = a })

cpvir2VirtualInterfaceName :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2VirtualInterfaceName =
    lens _cpvir2VirtualInterfaceName
        (\s a -> s { _cpvir2VirtualInterfaceName = a })

cpvir2VirtualInterfaceState :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2VirtualInterfaceState =
    lens _cpvir2VirtualInterfaceState
        (\s a -> s { _cpvir2VirtualInterfaceState = a })

cpvir2VirtualInterfaceType :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir2VirtualInterfaceType =
    lens _cpvir2VirtualInterfaceType
        (\s a -> s { _cpvir2VirtualInterfaceType = a })

cpvir2Vlan :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Int)
cpvir2Vlan = lens _cpvir2Vlan (\s a -> s { _cpvir2Vlan = a })

instance ToPath CreatePrivateVirtualInterface where
    toPath = const "/"

instance ToQuery CreatePrivateVirtualInterface where
    toQuery = const mempty

instance ToHeaders CreatePrivateVirtualInterface

instance ToJSON CreatePrivateVirtualInterface where
    toJSON CreatePrivateVirtualInterface{..} = object
        [ "connectionId"               .= _cpvi1ConnectionId
        , "newPrivateVirtualInterface" .= _cpvi1NewPrivateVirtualInterface
        ]

instance AWSRequest CreatePrivateVirtualInterface where
    type Sv CreatePrivateVirtualInterface = DirectConnect
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse

    request  = post "CreatePrivateVirtualInterface"
    response = jsonResponse

instance FromJSON CreatePrivateVirtualInterfaceResponse where
    parseJSON = withObject "CreatePrivateVirtualInterfaceResponse" $ \o -> CreatePrivateVirtualInterfaceResponse
        <$> o .:? "amazonAddress"
        <*> o .:? "asn"
        <*> o .:? "authKey"
        <*> o .:? "connectionId"
        <*> o .:? "customerAddress"
        <*> o .:? "customerRouterConfig"
        <*> o .:? "location"
        <*> o .:? "ownerAccount"
        <*> o .:  "routeFilterPrefixes"
        <*> o .:? "virtualGatewayId"
        <*> o .:? "virtualInterfaceId"
        <*> o .:? "virtualInterfaceName"
        <*> o .:? "virtualInterfaceState"
        <*> o .:? "virtualInterfaceType"
        <*> o .:? "vlan"


Some kind of operator / class to check the types whether to continue?
