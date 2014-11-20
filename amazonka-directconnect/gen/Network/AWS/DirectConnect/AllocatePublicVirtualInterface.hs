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

-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a public virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a public virtual
-- interface which will be owned by another AWS customer. Virtual interfaces
-- created using this function must be confirmed by the virtual interface
-- owner by calling ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface will be in 'Confirming' state, and will
-- not be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocatePublicVirtualInterface.html>
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
    (
    -- * Request
      AllocatePublicVirtualInterface
    -- ** Request constructor
    , allocatePublicVirtualInterface
    -- ** Request lenses
    , apvi1ConnectionId
    , apvi1NewPublicVirtualInterfaceAllocation
    , apvi1OwnerAccount

    -- * Response
    , AllocatePublicVirtualInterfaceResponse
    -- ** Response constructor
    , allocatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , apvir1AmazonAddress
    , apvir1Asn
    , apvir1AuthKey
    , apvir1ConnectionId
    , apvir1CustomerAddress
    , apvir1CustomerRouterConfig
    , apvir1Location
    , apvir1OwnerAccount
    , apvir1RouteFilterPrefixes
    , apvir1VirtualGatewayId
    , apvir1VirtualInterfaceId
    , apvir1VirtualInterfaceName
    , apvir1VirtualInterfaceState
    , apvir1VirtualInterfaceType
    , apvir1Vlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface
    { _apvi1ConnectionId                        :: Text
    , _apvi1NewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
    , _apvi1OwnerAccount                        :: Text
    } deriving (Eq, Show)

-- | 'AllocatePublicVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apvi1ConnectionId' @::@ 'Text'
--
-- * 'apvi1NewPublicVirtualInterfaceAllocation' @::@ 'NewPublicVirtualInterfaceAllocation'
--
-- * 'apvi1OwnerAccount' @::@ 'Text'
--
allocatePublicVirtualInterface :: Text -- ^ 'apvi1ConnectionId'
                               -> Text -- ^ 'apvi1OwnerAccount'
                               -> NewPublicVirtualInterfaceAllocation -- ^ 'apvi1NewPublicVirtualInterfaceAllocation'
                               -> AllocatePublicVirtualInterface
allocatePublicVirtualInterface p1 p2 p3 = AllocatePublicVirtualInterface
    { _apvi1ConnectionId                        = p1
    , _apvi1OwnerAccount                        = p2
    , _apvi1NewPublicVirtualInterfaceAllocation = p3
    }

-- | The connection ID on which the public virtual interface is provisioned.
-- Default: None.
apvi1ConnectionId :: Lens' AllocatePublicVirtualInterface Text
apvi1ConnectionId =
    lens _apvi1ConnectionId (\s a -> s { _apvi1ConnectionId = a })

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
apvi1NewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
apvi1NewPublicVirtualInterfaceAllocation =
    lens _apvi1NewPublicVirtualInterfaceAllocation
        (\s a -> s { _apvi1NewPublicVirtualInterfaceAllocation = a })

-- | The AWS account that will own the new public virtual interface. Default:
-- None.
apvi1OwnerAccount :: Lens' AllocatePublicVirtualInterface Text
apvi1OwnerAccount =
    lens _apvi1OwnerAccount (\s a -> s { _apvi1OwnerAccount = a })

data AllocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _apvir1AmazonAddress         :: Maybe Text
    , _apvir1Asn                   :: Maybe Int
    , _apvir1AuthKey               :: Maybe Text
    , _apvir1ConnectionId          :: Maybe Text
    , _apvir1CustomerAddress       :: Maybe Text
    , _apvir1CustomerRouterConfig  :: Maybe Text
    , _apvir1Location              :: Maybe Text
    , _apvir1OwnerAccount          :: Maybe Text
    , _apvir1RouteFilterPrefixes   :: List "routeFilterPrefixes" RouteFilterPrefix
    , _apvir1VirtualGatewayId      :: Maybe Text
    , _apvir1VirtualInterfaceId    :: Maybe Text
    , _apvir1VirtualInterfaceName  :: Maybe Text
    , _apvir1VirtualInterfaceState :: Maybe Text
    , _apvir1VirtualInterfaceType  :: Maybe Text
    , _apvir1Vlan                  :: Maybe Int
    } deriving (Eq, Show)

-- | 'AllocatePublicVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apvir1AmazonAddress' @::@ 'Maybe' 'Text'
--
-- * 'apvir1Asn' @::@ 'Maybe' 'Int'
--
-- * 'apvir1AuthKey' @::@ 'Maybe' 'Text'
--
-- * 'apvir1ConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'apvir1CustomerAddress' @::@ 'Maybe' 'Text'
--
-- * 'apvir1CustomerRouterConfig' @::@ 'Maybe' 'Text'
--
-- * 'apvir1Location' @::@ 'Maybe' 'Text'
--
-- * 'apvir1OwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'apvir1RouteFilterPrefixes' @::@ ['RouteFilterPrefix']
--
-- * 'apvir1VirtualGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'apvir1VirtualInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'apvir1VirtualInterfaceName' @::@ 'Maybe' 'Text'
--
-- * 'apvir1VirtualInterfaceState' @::@ 'Maybe' 'Text'
--
-- * 'apvir1VirtualInterfaceType' @::@ 'Maybe' 'Text'
--
-- * 'apvir1Vlan' @::@ 'Maybe' 'Int'
--
allocatePublicVirtualInterfaceResponse :: AllocatePublicVirtualInterfaceResponse
allocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _apvir1OwnerAccount          = Nothing
    , _apvir1VirtualInterfaceId    = Nothing
    , _apvir1Location              = Nothing
    , _apvir1ConnectionId          = Nothing
    , _apvir1VirtualInterfaceType  = Nothing
    , _apvir1VirtualInterfaceName  = Nothing
    , _apvir1Vlan                  = Nothing
    , _apvir1Asn                   = Nothing
    , _apvir1AuthKey               = Nothing
    , _apvir1AmazonAddress         = Nothing
    , _apvir1CustomerAddress       = Nothing
    , _apvir1VirtualInterfaceState = Nothing
    , _apvir1CustomerRouterConfig  = Nothing
    , _apvir1VirtualGatewayId      = Nothing
    , _apvir1RouteFilterPrefixes   = mempty
    }

apvir1AmazonAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1AmazonAddress =
    lens _apvir1AmazonAddress (\s a -> s { _apvir1AmazonAddress = a })

apvir1Asn :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Int)
apvir1Asn = lens _apvir1Asn (\s a -> s { _apvir1Asn = a })

apvir1AuthKey :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1AuthKey = lens _apvir1AuthKey (\s a -> s { _apvir1AuthKey = a })

apvir1ConnectionId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1ConnectionId =
    lens _apvir1ConnectionId (\s a -> s { _apvir1ConnectionId = a })

apvir1CustomerAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1CustomerAddress =
    lens _apvir1CustomerAddress (\s a -> s { _apvir1CustomerAddress = a })

-- | Information for generating the customer router configuration.
apvir1CustomerRouterConfig :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1CustomerRouterConfig =
    lens _apvir1CustomerRouterConfig
        (\s a -> s { _apvir1CustomerRouterConfig = a })

apvir1Location :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1Location = lens _apvir1Location (\s a -> s { _apvir1Location = a })

apvir1OwnerAccount :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1OwnerAccount =
    lens _apvir1OwnerAccount (\s a -> s { _apvir1OwnerAccount = a })

apvir1RouteFilterPrefixes :: Lens' AllocatePublicVirtualInterfaceResponse [RouteFilterPrefix]
apvir1RouteFilterPrefixes =
    lens _apvir1RouteFilterPrefixes
        (\s a -> s { _apvir1RouteFilterPrefixes = a })
            . _List

apvir1VirtualGatewayId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1VirtualGatewayId =
    lens _apvir1VirtualGatewayId (\s a -> s { _apvir1VirtualGatewayId = a })

apvir1VirtualInterfaceId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1VirtualInterfaceId =
    lens _apvir1VirtualInterfaceId
        (\s a -> s { _apvir1VirtualInterfaceId = a })

apvir1VirtualInterfaceName :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1VirtualInterfaceName =
    lens _apvir1VirtualInterfaceName
        (\s a -> s { _apvir1VirtualInterfaceName = a })

apvir1VirtualInterfaceState :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1VirtualInterfaceState =
    lens _apvir1VirtualInterfaceState
        (\s a -> s { _apvir1VirtualInterfaceState = a })

apvir1VirtualInterfaceType :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvir1VirtualInterfaceType =
    lens _apvir1VirtualInterfaceType
        (\s a -> s { _apvir1VirtualInterfaceType = a })

apvir1Vlan :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Int)
apvir1Vlan = lens _apvir1Vlan (\s a -> s { _apvir1Vlan = a })

instance ToPath AllocatePublicVirtualInterface where
    toPath = const "/"

instance ToQuery AllocatePublicVirtualInterface where
    toQuery = const mempty

instance ToHeaders AllocatePublicVirtualInterface

instance ToJSON AllocatePublicVirtualInterface where
    toJSON AllocatePublicVirtualInterface{..} = object
        [ "connectionId"                        .= _apvi1ConnectionId
        , "ownerAccount"                        .= _apvi1OwnerAccount
        , "newPublicVirtualInterfaceAllocation" .= _apvi1NewPublicVirtualInterfaceAllocation
        ]

instance AWSRequest AllocatePublicVirtualInterface where
    type Sv AllocatePublicVirtualInterface = DirectConnect
    type Rs AllocatePublicVirtualInterface = AllocatePublicVirtualInterfaceResponse

    request  = post "AllocatePublicVirtualInterface"
    response = jsonResponse

instance FromJSON AllocatePublicVirtualInterfaceResponse where
    parseJSON = withObject "AllocatePublicVirtualInterfaceResponse" $ \o -> AllocatePublicVirtualInterfaceResponse
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
