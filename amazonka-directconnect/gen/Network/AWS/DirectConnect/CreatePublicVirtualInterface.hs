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

-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
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

-- | Creates a new public virtual interface. A virtual interface is the VLAN that
-- transports AWS Direct Connect traffic. A public virtual interface supports
-- sending traffic to public services of AWS such as Amazon Simple Storage
-- Service (Amazon S3).
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePublicVirtualInterface.html>
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    (
    -- * Request
      CreatePublicVirtualInterface
    -- ** Request constructor
    , createPublicVirtualInterface
    -- ** Request lenses
    , cpviConnectionId
    , cpviNewPublicVirtualInterface

    -- * Response
    , CreatePublicVirtualInterfaceResponse
    -- ** Response constructor
    , createPublicVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirAmazonAddress
    , cpvirAsn
    , cpvirAuthKey
    , cpvirConnectionId
    , cpvirCustomerAddress
    , cpvirCustomerRouterConfig
    , cpvirLocation
    , cpvirOwnerAccount
    , cpvirRouteFilterPrefixes
    , cpvirVirtualGatewayId
    , cpvirVirtualInterfaceId
    , cpvirVirtualInterfaceName
    , cpvirVirtualInterfaceState
    , cpvirVirtualInterfaceType
    , cpvirVlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data CreatePublicVirtualInterface = CreatePublicVirtualInterface
    { _cpviConnectionId              :: Text
    , _cpviNewPublicVirtualInterface :: NewPublicVirtualInterface
    } deriving (Eq, Show)

-- | 'CreatePublicVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpviConnectionId' @::@ 'Text'
--
-- * 'cpviNewPublicVirtualInterface' @::@ 'NewPublicVirtualInterface'
--
createPublicVirtualInterface :: Text -- ^ 'cpviConnectionId'
                             -> NewPublicVirtualInterface -- ^ 'cpviNewPublicVirtualInterface'
                             -> CreatePublicVirtualInterface
createPublicVirtualInterface p1 p2 = CreatePublicVirtualInterface
    { _cpviConnectionId              = p1
    , _cpviNewPublicVirtualInterface = p2
    }

cpviConnectionId :: Lens' CreatePublicVirtualInterface Text
cpviConnectionId = lens _cpviConnectionId (\s a -> s { _cpviConnectionId = a })

-- | Detailed information for the public virtual interface to be created.
--
-- Default: None
cpviNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpviNewPublicVirtualInterface =
    lens _cpviNewPublicVirtualInterface
        (\s a -> s { _cpviNewPublicVirtualInterface = a })

data CreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _cpvirAmazonAddress         :: Maybe Text
    , _cpvirAsn                   :: Maybe Int
    , _cpvirAuthKey               :: Maybe Text
    , _cpvirConnectionId          :: Maybe Text
    , _cpvirCustomerAddress       :: Maybe Text
    , _cpvirCustomerRouterConfig  :: Maybe Text
    , _cpvirLocation              :: Maybe Text
    , _cpvirOwnerAccount          :: Maybe Text
    , _cpvirRouteFilterPrefixes   :: List "routeFilterPrefixes" RouteFilterPrefix
    , _cpvirVirtualGatewayId      :: Maybe Text
    , _cpvirVirtualInterfaceId    :: Maybe Text
    , _cpvirVirtualInterfaceName  :: Maybe Text
    , _cpvirVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _cpvirVirtualInterfaceType  :: Maybe Text
    , _cpvirVlan                  :: Maybe Int
    } deriving (Eq, Show)

-- | 'CreatePublicVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvirAmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'cpvirAsn' @::@ 'Maybe' 'Int'
--
-- * 'cpvirAuthKey' @::@ 'Maybe' 'Text'
--
-- * 'cpvirConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'cpvirCustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'cpvirCustomerRouterConfig' @::@ 'Maybe' 'Text'
--
-- * 'cpvirLocation' @::@ 'Maybe' 'Text'
--
-- * 'cpvirOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'cpvirRouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'cpvirVirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'cpvirVirtualInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'cpvirVirtualInterfaceName' @::@ 'Maybe' 'Text'
--
-- * 'cpvirVirtualInterfaceState' @::@ 'Maybe' 'VirtualInterfaceState'
--
-- * 'cpvirVirtualInterfaceType' @::@ 'Maybe' 'Text'
--
-- * 'cpvirVlan' @::@ 'Maybe' 'Int'
--
createPublicVirtualInterfaceResponse :: CreatePublicVirtualInterfaceResponse
createPublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _cpvirOwnerAccount          = Nothing
    , _cpvirVirtualInterfaceId    = Nothing
    , _cpvirLocation              = Nothing
    , _cpvirConnectionId          = Nothing
    , _cpvirVirtualInterfaceType  = Nothing
    , _cpvirVirtualInterfaceName  = Nothing
    , _cpvirVlan                  = Nothing
    , _cpvirAsn                   = Nothing
    , _cpvirAuthKey               = Nothing
    , _cpvirAmazonAddress         = Nothing
    , _cpvirCustomerAddress       = Nothing
    , _cpvirVirtualInterfaceState = Nothing
    , _cpvirCustomerRouterConfig  = Nothing
    , _cpvirVirtualGatewayId      = Nothing
    , _cpvirRouteFilterPrefixes   = mempty
    }

cpvirAmazonAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirAmazonAddress =
    lens _cpvirAmazonAddress (\s a -> s { _cpvirAmazonAddress = a })

cpvirAsn :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Int)
cpvirAsn = lens _cpvirAsn (\s a -> s { _cpvirAsn = a })

cpvirAuthKey :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirAuthKey = lens _cpvirAuthKey (\s a -> s { _cpvirAuthKey = a })

cpvirConnectionId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirConnectionId =
    lens _cpvirConnectionId (\s a -> s { _cpvirConnectionId = a })

cpvirCustomerAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirCustomerAddress =
    lens _cpvirCustomerAddress (\s a -> s { _cpvirCustomerAddress = a })

-- | Information for generating the customer router configuration.
cpvirCustomerRouterConfig :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirCustomerRouterConfig =
    lens _cpvirCustomerRouterConfig
        (\s a -> s { _cpvirCustomerRouterConfig = a })

cpvirLocation :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirLocation = lens _cpvirLocation (\s a -> s { _cpvirLocation = a })

cpvirOwnerAccount :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirOwnerAccount =
    lens _cpvirOwnerAccount (\s a -> s { _cpvirOwnerAccount = a })

cpvirRouteFilterPrefixes :: Lens' CreatePublicVirtualInterfaceResponse [RouteFilterPrefix]
cpvirRouteFilterPrefixes =
    lens _cpvirRouteFilterPrefixes
        (\s a -> s { _cpvirRouteFilterPrefixes = a })
            . _List

cpvirVirtualGatewayId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirVirtualGatewayId =
    lens _cpvirVirtualGatewayId (\s a -> s { _cpvirVirtualGatewayId = a })

cpvirVirtualInterfaceId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirVirtualInterfaceId =
    lens _cpvirVirtualInterfaceId (\s a -> s { _cpvirVirtualInterfaceId = a })

cpvirVirtualInterfaceName :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirVirtualInterfaceName =
    lens _cpvirVirtualInterfaceName
        (\s a -> s { _cpvirVirtualInterfaceName = a })

cpvirVirtualInterfaceState :: Lens' CreatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirVirtualInterfaceState =
    lens _cpvirVirtualInterfaceState
        (\s a -> s { _cpvirVirtualInterfaceState = a })

cpvirVirtualInterfaceType :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirVirtualInterfaceType =
    lens _cpvirVirtualInterfaceType
        (\s a -> s { _cpvirVirtualInterfaceType = a })

cpvirVlan :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Int)
cpvirVlan = lens _cpvirVlan (\s a -> s { _cpvirVlan = a })

instance ToPath CreatePublicVirtualInterface where
    toPath = const "/"

instance ToQuery CreatePublicVirtualInterface where
    toQuery = const mempty

instance ToHeaders CreatePublicVirtualInterface

instance ToJSON CreatePublicVirtualInterface where
    toJSON CreatePublicVirtualInterface{..} = object
        [ "connectionId"              .= _cpviConnectionId
        , "newPublicVirtualInterface" .= _cpviNewPublicVirtualInterface
        ]

instance AWSRequest CreatePublicVirtualInterface where
    type Sv CreatePublicVirtualInterface = DirectConnect
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse

    request  = post "CreatePublicVirtualInterface"
    response = jsonResponse

instance FromJSON CreatePublicVirtualInterfaceResponse where
    parseJSON = withObject "CreatePublicVirtualInterfaceResponse" $ \o -> CreatePublicVirtualInterfaceResponse
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
