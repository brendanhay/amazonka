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

-- Module      : Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept ownership of a private virtual interface created by another
-- customer. After the virtual interface owner calls this function, the
-- virtual interface will be created and attached to the given virtual private
-- gateway, and will be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmPrivateVirtualInterface.html>
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    (
    -- * Request
      ConfirmPrivateVirtualInterface
    -- ** Request constructor
    , confirmPrivateVirtualInterface
    -- ** Request lenses
    , cpviVirtualGatewayId
    , cpviVirtualInterfaceId

    -- * Response
    , ConfirmPrivateVirtualInterfaceResponse
    -- ** Response constructor
    , confirmPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvir1VirtualInterfaceState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface
    { _cpviVirtualGatewayId   :: Text
    , _cpviVirtualInterfaceId :: Text
    } deriving (Eq, Ord, Show)

-- | 'ConfirmPrivateVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpviVirtualGatewayId' @::@ 'Text'
--
-- * 'cpviVirtualInterfaceId' @::@ 'Text'
--
confirmPrivateVirtualInterface :: Text -- ^ 'cpviVirtualInterfaceId'
                               -> Text -- ^ 'cpviVirtualGatewayId'
                               -> ConfirmPrivateVirtualInterface
confirmPrivateVirtualInterface p1 p2 = ConfirmPrivateVirtualInterface
    { _cpviVirtualInterfaceId = p1
    , _cpviVirtualGatewayId   = p2
    }

-- | ID of the virtual private gateway that will be attached to the virtual
-- interface. A virtual private gateway can be managed via the Amazon
-- Virtual Private Cloud (VPC) console or the EC2 CreateVpnGateway action.
-- Default: None.
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualGatewayId =
    lens _cpviVirtualGatewayId (\s a -> s { _cpviVirtualGatewayId = a })

cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId =
    lens _cpviVirtualInterfaceId (\s a -> s { _cpviVirtualInterfaceId = a })

newtype ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse
    { _cpvir1VirtualInterfaceState :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ConfirmPrivateVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvir1VirtualInterfaceState' @::@ 'Maybe' 'Text'
--
confirmPrivateVirtualInterfaceResponse :: ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse
    { _cpvir1VirtualInterfaceState = Nothing
    }

cpvir1VirtualInterfaceState :: Lens' ConfirmPrivateVirtualInterfaceResponse (Maybe Text)
cpvir1VirtualInterfaceState =
    lens _cpvir1VirtualInterfaceState
        (\s a -> s { _cpvir1VirtualInterfaceState = a })

instance ToPath ConfirmPrivateVirtualInterface where
    toPath = const "/"

instance ToQuery ConfirmPrivateVirtualInterface where
    toQuery = const mempty

instance ToHeaders ConfirmPrivateVirtualInterface

instance ToJSON ConfirmPrivateVirtualInterface where
    toJSON ConfirmPrivateVirtualInterface{..} = object
        [ "virtualInterfaceId" .= _cpviVirtualInterfaceId
        , "virtualGatewayId"   .= _cpviVirtualGatewayId
        ]

instance AWSRequest ConfirmPrivateVirtualInterface where
    type Sv ConfirmPrivateVirtualInterface = DirectConnect
    type Rs ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterfaceResponse

    request  = post "ConfirmPrivateVirtualInterface"
    response = jsonResponse

instance FromJSON ConfirmPrivateVirtualInterfaceResponse where
    parseJSON = withObject "ConfirmPrivateVirtualInterfaceResponse" $ \o -> ConfirmPrivateVirtualInterfaceResponse
        <$> o .:? "virtualInterfaceState"
