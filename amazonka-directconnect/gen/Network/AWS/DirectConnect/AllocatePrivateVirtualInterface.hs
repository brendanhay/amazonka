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

-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a private virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a private
-- virtual interface which will be owned by another AWS customer. Virtual
-- interfaces created using this function must be confirmed by the virtual
-- interface owner by calling ConfirmPrivateVirtualInterface. Until this step
-- has been completed, the virtual interface will be in 'Confirming' state,
-- and will not be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocatePrivateVirtualInterface.html>
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    (
    -- * Request
      AllocatePrivateVirtualInterface
    -- ** Request constructor
    , allocatePrivateVirtualInterface
    -- ** Request lenses
    , apviConnectionId
    , apviNewPrivateVirtualInterfaceAllocation
    , apviOwnerAccount

    -- * Response
    , AllocatePrivateVirtualInterfaceResponse
    -- ** Response constructor
    , allocatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , apvirAmazonAddress
    , apvirAsn
    , apvirAuthKey
    , apvirConnectionId
    , apvirCustomerAddress
    , apvirCustomerRouterConfig
    , apvirLocation
    , apvirOwnerAccount
    , apvirRouteFilterPrefixes
    , apvirVirtualGatewayId
    , apvirVirtualInterfaceId
    , apvirVirtualInterfaceName
    , apvirVirtualInterfaceState
    , apvirVirtualInterfaceType
    , apvirVlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { _apviConnectionId                         :: Text
    , _apviNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
    , _apviOwnerAccount                         :: Text
    } deriving (Eq, Show)

-- | 'AllocatePrivateVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apviConnectionId' @::@ 'Text'
--
-- * 'apviNewPrivateVirtualInterfaceAllocation' @::@ 'NewPrivateVirtualInterfaceAllocation'
--
-- * 'apviOwnerAccount' @::@ 'Text'
--
allocatePrivateVirtualInterface :: Text -- ^ 'apviConnectionId'
                                -> Text -- ^ 'apviOwnerAccount'
                                -> NewPrivateVirtualInterfaceAllocation -- ^ 'apviNewPrivateVirtualInterfaceAllocation'
                                -> AllocatePrivateVirtualInterface
allocatePrivateVirtualInterface p1 p2 p3 = AllocatePrivateVirtualInterface
    { _apviConnectionId                         = p1
    , _apviOwnerAccount                         = p2
    , _apviNewPrivateVirtualInterfaceAllocation = p3
    }

-- | The connection ID on which the private virtual interface is provisioned.
-- Default: None.
apviConnectionId :: Lens' AllocatePrivateVirtualInterface Text
apviConnectionId = lens _apviConnectionId (\s a -> s { _apviConnectionId = a })

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
apviNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation =
    lens _apviNewPrivateVirtualInterfaceAllocation
        (\s a -> s { _apviNewPrivateVirtualInterfaceAllocation = a })

-- | The AWS account that will own the new private virtual interface. Default:
-- None.
apviOwnerAccount :: Lens' AllocatePrivateVirtualInterface Text
apviOwnerAccount = lens _apviOwnerAccount (\s a -> s { _apviOwnerAccount = a })

data AllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _apvirAmazonAddress         :: Maybe Text
    , _apvirAsn                   :: Maybe Int
    , _apvirAuthKey               :: Maybe Text
    , _apvirConnectionId          :: Maybe Text
    , _apvirCustomerAddress       :: Maybe Text
    , _apvirCustomerRouterConfig  :: Maybe Text
    , _apvirLocation              :: Maybe Text
    , _apvirOwnerAccount          :: Maybe Text
    , _apvirRouteFilterPrefixes   :: List "routeFilterPrefixes" RouteFilterPrefix
    , _apvirVirtualGatewayId      :: Maybe Text
    , _apvirVirtualInterfaceId    :: Maybe Text
    , _apvirVirtualInterfaceName  :: Maybe Text
    , _apvirVirtualInterfaceState :: Maybe Text
    , _apvirVirtualInterfaceType  :: Maybe Text
    , _apvirVlan                  :: Maybe Int
    } deriving (Eq, Show)

-- | 'AllocatePrivateVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apvirAmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'apvirAsn' @::@ 'Maybe' 'Int'
--
-- * 'apvirAuthKey' @::@ 'Maybe' 'Text'
--
-- * 'apvirConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'apvirCustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'apvirCustomerRouterConfig' @::@ 'Maybe' 'Text'
--
-- * 'apvirLocation' @::@ 'Maybe' 'Text'
--
-- * 'apvirOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'apvirRouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'apvirVirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'apvirVirtualInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'apvirVirtualInterfaceName' @::@ 'Maybe' 'Text'
--
-- * 'apvirVirtualInterfaceState' @::@ 'Maybe' 'Text'
--
-- * 'apvirVirtualInterfaceType' @::@ 'Maybe' 'Text'
--
-- * 'apvirVlan' @::@ 'Maybe' 'Int'
--
allocatePrivateVirtualInterfaceResponse :: AllocatePrivateVirtualInterfaceResponse
allocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _apvirOwnerAccount          = Nothing
    , _apvirVirtualInterfaceId    = Nothing
    , _apvirLocation              = Nothing
    , _apvirConnectionId          = Nothing
    , _apvirVirtualInterfaceType  = Nothing
    , _apvirVirtualInterfaceName  = Nothing
    , _apvirVlan                  = Nothing
    , _apvirAsn                   = Nothing
    , _apvirAuthKey               = Nothing
    , _apvirAmazonAddress         = Nothing
    , _apvirCustomerAddress       = Nothing
    , _apvirVirtualInterfaceState = Nothing
    , _apvirCustomerRouterConfig  = Nothing
    , _apvirVirtualGatewayId      = Nothing
    , _apvirRouteFilterPrefixes   = mempty
    }

apvirAmazonAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirAmazonAddress =
    lens _apvirAmazonAddress (\s a -> s { _apvirAmazonAddress = a })

apvirAsn :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Int)
apvirAsn = lens _apvirAsn (\s a -> s { _apvirAsn = a })

apvirAuthKey :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirAuthKey = lens _apvirAuthKey (\s a -> s { _apvirAuthKey = a })

apvirConnectionId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirConnectionId =
    lens _apvirConnectionId (\s a -> s { _apvirConnectionId = a })

apvirCustomerAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirCustomerAddress =
    lens _apvirCustomerAddress (\s a -> s { _apvirCustomerAddress = a })

-- | Information for generating the customer router configuration.
apvirCustomerRouterConfig :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirCustomerRouterConfig =
    lens _apvirCustomerRouterConfig
        (\s a -> s { _apvirCustomerRouterConfig = a })

apvirLocation :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirLocation = lens _apvirLocation (\s a -> s { _apvirLocation = a })

apvirOwnerAccount :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirOwnerAccount =
    lens _apvirOwnerAccount (\s a -> s { _apvirOwnerAccount = a })

apvirRouteFilterPrefixes :: Lens' AllocatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
apvirRouteFilterPrefixes =
    lens _apvirRouteFilterPrefixes
        (\s a -> s { _apvirRouteFilterPrefixes = a })
            . _List

apvirVirtualGatewayId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualGatewayId =
    lens _apvirVirtualGatewayId (\s a -> s { _apvirVirtualGatewayId = a })

apvirVirtualInterfaceId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceId =
    lens _apvirVirtualInterfaceId (\s a -> s { _apvirVirtualInterfaceId = a })

apvirVirtualInterfaceName :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceName =
    lens _apvirVirtualInterfaceName
        (\s a -> s { _apvirVirtualInterfaceName = a })

apvirVirtualInterfaceState :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceState =
    lens _apvirVirtualInterfaceState
        (\s a -> s { _apvirVirtualInterfaceState = a })

apvirVirtualInterfaceType :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceType =
    lens _apvirVirtualInterfaceType
        (\s a -> s { _apvirVirtualInterfaceType = a })

apvirVlan :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Int)
apvirVlan = lens _apvirVlan (\s a -> s { _apvirVlan = a })

instance ToPath AllocatePrivateVirtualInterface where
    toPath = const "/"

instance ToQuery AllocatePrivateVirtualInterface where
    toQuery = const mempty

instance ToHeaders AllocatePrivateVirtualInterface

instance ToJSON AllocatePrivateVirtualInterface where
    toJSON AllocatePrivateVirtualInterface{..} = object
        [ "connectionId"                         .= _apviConnectionId
        , "ownerAccount"                         .= _apviOwnerAccount
        , "newPrivateVirtualInterfaceAllocation" .= _apviNewPrivateVirtualInterfaceAllocation
        ]

instance AWSRequest AllocatePrivateVirtualInterface where
    type Sv AllocatePrivateVirtualInterface = DirectConnect
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse

    request  = post "AllocatePrivateVirtualInterface"
    response = jsonResponse

instance FromJSON AllocatePrivateVirtualInterfaceResponse where
    parseJSON = withObject "AllocatePrivateVirtualInterfaceResponse" $ \o -> AllocatePrivateVirtualInterfaceResponse
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
