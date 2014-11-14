{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a hosted connection on an interconnect. Allocates a VLAN number and
-- a specified amount of bandwidth for use by a hosted connection on the given
-- interconnect.
module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
    (
    -- * Request
      AllocateConnectionOnInterconnect
    -- ** Request constructor
    , allocateConnectionOnInterconnect
    -- ** Request lenses
    , acoiBandwidth
    , acoiConnectionName
    , acoiInterconnectId
    , acoiOwnerAccount
    , acoiVlan

    -- * Response
    , AllocateConnectionOnInterconnectResponse
    -- ** Response constructor
    , allocateConnectionOnInterconnectResponse
    -- ** Response lenses
    , acoirBandwidth
    , acoirConnectionId
    , acoirConnectionName
    , acoirConnectionState
    , acoirLocation
    , acoirOwnerAccount
    , acoirPartnerName
    , acoirRegion
    , acoirVlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DirectConnect.Types

data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { _acoiBandwidth      :: Text
    , _acoiConnectionName :: Text
    , _acoiInterconnectId :: Text
    , _acoiOwnerAccount   :: Text
    , _acoiVlan           :: Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'AllocateConnectionOnInterconnect' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoiBandwidth' @::@ 'Text'
--
-- * 'acoiConnectionName' @::@ 'Text'
--
-- * 'acoiInterconnectId' @::@ 'Text'
--
-- * 'acoiOwnerAccount' @::@ 'Text'
--
-- * 'acoiVlan' @::@ 'Int'
--
allocateConnectionOnInterconnect :: Text -- ^ 'acoiBandwidth'
                                 -> Text -- ^ 'acoiConnectionName'
                                 -> Text -- ^ 'acoiOwnerAccount'
                                 -> Text -- ^ 'acoiInterconnectId'
                                 -> Int -- ^ 'acoiVlan'
                                 -> AllocateConnectionOnInterconnect
allocateConnectionOnInterconnect p1 p2 p3 p4 p5 = AllocateConnectionOnInterconnect
    { _acoiBandwidth      = p1
    , _acoiConnectionName = p2
    , _acoiOwnerAccount   = p3
    , _acoiInterconnectId = p4
    , _acoiVlan           = p5
    }

-- | Bandwidth of the connection. Example: "500Mbps" Default: None.
acoiBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoiBandwidth = lens _acoiBandwidth (\s a -> s { _acoiBandwidth = a })

-- | Name of the provisioned connection. Example: "500M Connection to AWS"
-- Default: None.
acoiConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoiConnectionName =
    lens _acoiConnectionName (\s a -> s { _acoiConnectionName = a })

-- | ID of the interconnect on which the connection will be provisioned.
-- Example: dxcon-456abc78 Default: None.
acoiInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoiInterconnectId =
    lens _acoiInterconnectId (\s a -> s { _acoiInterconnectId = a })

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned. Example: 123443215678 Default: None.
acoiOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoiOwnerAccount = lens _acoiOwnerAccount (\s a -> s { _acoiOwnerAccount = a })

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default:
-- None.
acoiVlan :: Lens' AllocateConnectionOnInterconnect Int
acoiVlan = lens _acoiVlan (\s a -> s { _acoiVlan = a })

instance ToPath AllocateConnectionOnInterconnect where
    toPath = const "/"

instance ToQuery AllocateConnectionOnInterconnect where
    toQuery = const mempty

instance ToHeaders AllocateConnectionOnInterconnect

instance ToBody AllocateConnectionOnInterconnect where
    toBody = toBody . encode . _acoiBandwidth

data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _acoirBandwidth       :: Maybe Text
    , _acoirConnectionId    :: Maybe Text
    , _acoirConnectionName  :: Maybe Text
    , _acoirConnectionState :: Maybe Text
    , _acoirLocation        :: Maybe Text
    , _acoirOwnerAccount    :: Maybe Text
    , _acoirPartnerName     :: Maybe Text
    , _acoirRegion          :: Maybe Text
    , _acoirVlan            :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'AllocateConnectionOnInterconnectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoirBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'acoirConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'acoirConnectionName' @::@ 'Maybe' 'Text'
--
-- * 'acoirConnectionState' @::@ 'Maybe' 'Text'
--
-- * 'acoirLocation' @::@ 'Maybe' 'Text'
--
-- * 'acoirOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'acoirPartnerName' @::@ 'Maybe' 'Text'
--
-- * 'acoirRegion' @::@ 'Maybe' 'Text'
--
-- * 'acoirVlan' @::@ 'Maybe' 'Int'
--
allocateConnectionOnInterconnectResponse :: AllocateConnectionOnInterconnectResponse
allocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _acoirOwnerAccount    = Nothing
    , _acoirConnectionId    = Nothing
    , _acoirConnectionName  = Nothing
    , _acoirConnectionState = Nothing
    , _acoirRegion          = Nothing
    , _acoirLocation        = Nothing
    , _acoirBandwidth       = Nothing
    , _acoirVlan            = Nothing
    , _acoirPartnerName     = Nothing
    }

-- | Bandwidth of the connection. Example: 1Gbps (for regular connections), or
-- 500Mbps (for hosted connections) Default: None.
acoirBandwidth :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirBandwidth = lens _acoirBandwidth (\s a -> s { _acoirBandwidth = a })

acoirConnectionId :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirConnectionId =
    lens _acoirConnectionId (\s a -> s { _acoirConnectionId = a })

acoirConnectionName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirConnectionName =
    lens _acoirConnectionName (\s a -> s { _acoirConnectionName = a })

acoirConnectionState :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirConnectionState =
    lens _acoirConnectionState (\s a -> s { _acoirConnectionState = a })

acoirLocation :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirLocation = lens _acoirLocation (\s a -> s { _acoirLocation = a })

acoirOwnerAccount :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirOwnerAccount =
    lens _acoirOwnerAccount (\s a -> s { _acoirOwnerAccount = a })

acoirPartnerName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirPartnerName = lens _acoirPartnerName (\s a -> s { _acoirPartnerName = a })

acoirRegion :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirRegion = lens _acoirRegion (\s a -> s { _acoirRegion = a })

acoirVlan :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Int)
acoirVlan = lens _acoirVlan (\s a -> s { _acoirVlan = a })

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request  = post
    response = jsonResponse $ \h o -> AllocateConnectionOnInterconnectResponse
        <$> o .: "bandwidth"
        <*> o .: "connectionId"
        <*> o .: "connectionName"
        <*> o .: "connectionState"
        <*> o .: "location"
        <*> o .: "ownerAccount"
        <*> o .: "partnerName"
        <*> o .: "region"
        <*> o .: "vlan"
