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

-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new interconnect between a AWS Direct Connect partner's network
-- and a specific AWS Direct Connect location. An interconnect is a connection
-- which is capable of hosting other connections. The AWS Direct Connect
-- partner can use an interconnect to provide sub-1Gbps AWS Direct Connect
-- service to tier 2 customers who do not have their own connections. Like a
-- standard connection, an interconnect links the AWS Direct Connect partner's
-- network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps
-- Ethernet fiber-optic cable. One end is connected to the partner's router,
-- the other to an AWS Direct Connect router. For each end customer, the AWS
-- Direct Connect partner provisions a connection on their interconnect by
-- calling AllocateConnectionOnInterconnect. The end customer can then connect
-- to AWS resources by creating a virtual interface on their connection, using
-- the VLAN assigned to them by the AWS Direct Connect partner.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreateInterconnect.html>
module Network.AWS.DirectConnect.CreateInterconnect
    (
    -- * Request
      CreateInterconnect
    -- ** Request constructor
    , createInterconnect
    -- ** Request lenses
    , ciBandwidth
    , ciInterconnectName
    , ciLocation

    -- * Response
    , CreateInterconnectResponse
    -- ** Response constructor
    , createInterconnectResponse
    -- ** Response lenses
    , cirBandwidth
    , cirInterconnectId
    , cirInterconnectName
    , cirInterconnectState
    , cirLocation
    , cirRegion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data CreateInterconnect = CreateInterconnect
    { _ciBandwidth        :: Text
    , _ciInterconnectName :: Text
    , _ciLocation         :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateInterconnect' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciBandwidth' @::@ 'Text'
--
-- * 'ciInterconnectName' @::@ 'Text'
--
-- * 'ciLocation' @::@ 'Text'
--
createInterconnect :: Text -- ^ 'ciInterconnectName'
                   -> Text -- ^ 'ciBandwidth'
                   -> Text -- ^ 'ciLocation'
                   -> CreateInterconnect
createInterconnect p1 p2 p3 = CreateInterconnect
    { _ciInterconnectName = p1
    , _ciBandwidth        = p2
    , _ciLocation         = p3
    }

-- | The port bandwidth Example: 1Gbps Default: None Available values:
-- 1Gbps,10Gbps.
ciBandwidth :: Lens' CreateInterconnect Text
ciBandwidth = lens _ciBandwidth (\s a -> s { _ciBandwidth = a })

-- | The name of the interconnect. Example: "1G Interconnect to AWS" Default:
-- None.
ciInterconnectName :: Lens' CreateInterconnect Text
ciInterconnectName =
    lens _ciInterconnectName (\s a -> s { _ciInterconnectName = a })

-- | Where the interconnect is located Example: EqSV5 Default: None.
ciLocation :: Lens' CreateInterconnect Text
ciLocation = lens _ciLocation (\s a -> s { _ciLocation = a })

data CreateInterconnectResponse = CreateInterconnectResponse
    { _cirBandwidth         :: Maybe Text
    , _cirInterconnectId    :: Maybe Text
    , _cirInterconnectName  :: Maybe Text
    , _cirInterconnectState :: Maybe Text
    , _cirLocation          :: Maybe Text
    , _cirRegion            :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateInterconnectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'cirInterconnectId' @::@ 'Maybe' 'Text'
--
-- * 'cirInterconnectName' @::@ 'Maybe' 'Text'
--
-- * 'cirInterconnectState' @::@ 'Maybe' 'Text'
--
-- * 'cirLocation' @::@ 'Maybe' 'Text'
--
-- * 'cirRegion' @::@ 'Maybe' 'Text'
--
createInterconnectResponse :: CreateInterconnectResponse
createInterconnectResponse = CreateInterconnectResponse
    { _cirInterconnectId    = Nothing
    , _cirInterconnectName  = Nothing
    , _cirInterconnectState = Nothing
    , _cirRegion            = Nothing
    , _cirLocation          = Nothing
    , _cirBandwidth         = Nothing
    }

cirBandwidth :: Lens' CreateInterconnectResponse (Maybe Text)
cirBandwidth = lens _cirBandwidth (\s a -> s { _cirBandwidth = a })

cirInterconnectId :: Lens' CreateInterconnectResponse (Maybe Text)
cirInterconnectId =
    lens _cirInterconnectId (\s a -> s { _cirInterconnectId = a })

cirInterconnectName :: Lens' CreateInterconnectResponse (Maybe Text)
cirInterconnectName =
    lens _cirInterconnectName (\s a -> s { _cirInterconnectName = a })

cirInterconnectState :: Lens' CreateInterconnectResponse (Maybe Text)
cirInterconnectState =
    lens _cirInterconnectState (\s a -> s { _cirInterconnectState = a })

cirLocation :: Lens' CreateInterconnectResponse (Maybe Text)
cirLocation = lens _cirLocation (\s a -> s { _cirLocation = a })

cirRegion :: Lens' CreateInterconnectResponse (Maybe Text)
cirRegion = lens _cirRegion (\s a -> s { _cirRegion = a })

instance ToPath CreateInterconnect where
    toPath = const "/"

instance ToQuery CreateInterconnect where
    toQuery = const mempty

instance ToHeaders CreateInterconnect

instance ToJSON CreateInterconnect where
    toJSON CreateInterconnect{..} = object
        [ "interconnectName" .= _ciInterconnectName
        , "bandwidth"        .= _ciBandwidth
        , "location"         .= _ciLocation
        ]

instance AWSRequest CreateInterconnect where
    type Sv CreateInterconnect = DirectConnect
    type Rs CreateInterconnect = CreateInterconnectResponse

    request  = post "CreateInterconnect"
    response = jsonResponse

instance FromJSON CreateInterconnectResponse where
    parseJSON = withObject "CreateInterconnectResponse" $ \o -> CreateInterconnectResponse
        <$> o .:? "bandwidth"
        <*> o .:? "interconnectId"
        <*> o .:? "interconnectName"
        <*> o .:? "interconnectState"
        <*> o .:? "location"
        <*> o .:? "region"
