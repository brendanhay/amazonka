{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new connection between the customer network and a specific AWS
-- Direct Connect location. A connection links your internal network to an AWS
-- Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet
-- fiber-optic cable. One end of the cable is connected to your router, the
-- other to an AWS Direct Connect router. An AWS Direct Connect location
-- provides access to Amazon Web Services in the region it is associated with.
-- You can establish connections with AWS Direct Connect locations in multiple
-- regions, but a connection in one region does not provide connectivity to
-- other regions.
module Network.AWS.DirectConnect.CreateConnection
    (
    -- * Request
      CreateConnection
    -- ** Request constructor
    , createConnection
    -- ** Request lenses
    , ccBandwidth
    , ccConnectionName
    , ccLocation

    -- * Response
    , CreateConnectionResponse
    -- ** Response constructor
    , createConnectionResponse
    -- ** Response lenses
    , ccrBandwidth
    , ccrConnectionId
    , ccrConnectionName
    , ccrConnectionState
    , ccrLocation
    , ccrOwnerAccount
    , ccrPartnerName
    , ccrRegion
    , ccrVlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data CreateConnection = CreateConnection
    { _ccBandwidth      :: Text
    , _ccConnectionName :: Text
    , _ccLocation       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccBandwidth' @::@ 'Text'
--
-- * 'ccConnectionName' @::@ 'Text'
--
-- * 'ccLocation' @::@ 'Text'
--
createConnection :: Text -- ^ 'ccLocation'
                 -> Text -- ^ 'ccBandwidth'
                 -> Text -- ^ 'ccConnectionName'
                 -> CreateConnection
createConnection p1 p2 p3 = CreateConnection
    { _ccLocation       = p1
    , _ccBandwidth      = p2
    , _ccConnectionName = p3
    }

ccBandwidth :: Lens' CreateConnection Text
ccBandwidth = lens _ccBandwidth (\s a -> s { _ccBandwidth = a })

ccConnectionName :: Lens' CreateConnection Text
ccConnectionName = lens _ccConnectionName (\s a -> s { _ccConnectionName = a })

ccLocation :: Lens' CreateConnection Text
ccLocation = lens _ccLocation (\s a -> s { _ccLocation = a })

data CreateConnectionResponse = CreateConnectionResponse
    { _ccrBandwidth       :: Maybe Text
    , _ccrConnectionId    :: Maybe Text
    , _ccrConnectionName  :: Maybe Text
    , _ccrConnectionState :: Maybe Text
    , _ccrLocation        :: Maybe Text
    , _ccrOwnerAccount    :: Maybe Text
    , _ccrPartnerName     :: Maybe Text
    , _ccrRegion          :: Maybe Text
    , _ccrVlan            :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'ccrConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'ccrConnectionName' @::@ 'Maybe' 'Text'
--
-- * 'ccrConnectionState' @::@ 'Maybe' 'Text'
--
-- * 'ccrLocation' @::@ 'Maybe' 'Text'
--
-- * 'ccrOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'ccrPartnerName' @::@ 'Maybe' 'Text'
--
-- * 'ccrRegion' @::@ 'Maybe' 'Text'
--
-- * 'ccrVlan' @::@ 'Maybe' 'Int'
--
createConnectionResponse :: CreateConnectionResponse
createConnectionResponse = CreateConnectionResponse
    { _ccrOwnerAccount    = Nothing
    , _ccrConnectionId    = Nothing
    , _ccrConnectionName  = Nothing
    , _ccrConnectionState = Nothing
    , _ccrRegion          = Nothing
    , _ccrLocation        = Nothing
    , _ccrBandwidth       = Nothing
    , _ccrVlan            = Nothing
    , _ccrPartnerName     = Nothing
    }

-- | Bandwidth of the connection. Example: 1Gbps (for regular connections), or
-- 500Mbps (for hosted connections) Default: None.
ccrBandwidth :: Lens' CreateConnectionResponse (Maybe Text)
ccrBandwidth = lens _ccrBandwidth (\s a -> s { _ccrBandwidth = a })

ccrConnectionId :: Lens' CreateConnectionResponse (Maybe Text)
ccrConnectionId = lens _ccrConnectionId (\s a -> s { _ccrConnectionId = a })

ccrConnectionName :: Lens' CreateConnectionResponse (Maybe Text)
ccrConnectionName =
    lens _ccrConnectionName (\s a -> s { _ccrConnectionName = a })

ccrConnectionState :: Lens' CreateConnectionResponse (Maybe Text)
ccrConnectionState =
    lens _ccrConnectionState (\s a -> s { _ccrConnectionState = a })

ccrLocation :: Lens' CreateConnectionResponse (Maybe Text)
ccrLocation = lens _ccrLocation (\s a -> s { _ccrLocation = a })

ccrOwnerAccount :: Lens' CreateConnectionResponse (Maybe Text)
ccrOwnerAccount = lens _ccrOwnerAccount (\s a -> s { _ccrOwnerAccount = a })

ccrPartnerName :: Lens' CreateConnectionResponse (Maybe Text)
ccrPartnerName = lens _ccrPartnerName (\s a -> s { _ccrPartnerName = a })

ccrRegion :: Lens' CreateConnectionResponse (Maybe Text)
ccrRegion = lens _ccrRegion (\s a -> s { _ccrRegion = a })

ccrVlan :: Lens' CreateConnectionResponse (Maybe Int)
ccrVlan = lens _ccrVlan (\s a -> s { _ccrVlan = a })

instance AWSRequest CreateConnection where
    type Sv CreateConnection = DirectConnect
    type Rs CreateConnection = CreateConnectionResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateConnectionResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath CreateConnection where
    toPath = const "/"

instance ToHeaders CreateConnection

instance ToQuery CreateConnection where
    toQuery = const mempty

instance ToJSON CreateConnection where
    toJSON = genericToJSON jsonOptions
