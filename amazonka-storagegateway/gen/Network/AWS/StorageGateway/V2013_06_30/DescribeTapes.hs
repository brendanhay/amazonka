{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DescribeTapes
    (
    -- * Request
      DescribeTapes
    -- ** Request constructor
    , mkDescribeTapes
    -- ** Request lenses
    , dt1GatewayARN
    , dt1TapeARNs
    , dt1Marker
    , dt1Limit

    -- * Response
    , DescribeTapesResponse
    -- ** Response lenses
    , dtrsrsTapes
    , dtrsrsMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeTapes = DescribeTapes
    { _dt1GatewayARN :: Text
    , _dt1TapeARNs :: [Text]
    , _dt1Marker :: Maybe Text
    , _dt1Limit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapes' request.
mkDescribeTapes :: Text -- ^ 'dt1GatewayARN'
                -> DescribeTapes
mkDescribeTapes p1 = DescribeTapes
    { _dt1GatewayARN = p1
    , _dt1TapeARNs = mempty
    , _dt1Marker = Nothing
    , _dt1Limit = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dt1GatewayARN :: Lens' DescribeTapes Text
dt1GatewayARN = lens _dt1GatewayARN (\s a -> s { _dt1GatewayARN = a })

dt1TapeARNs :: Lens' DescribeTapes [Text]
dt1TapeARNs = lens _dt1TapeARNs (\s a -> s { _dt1TapeARNs = a })

dt1Marker :: Lens' DescribeTapes (Maybe Text)
dt1Marker = lens _dt1Marker (\s a -> s { _dt1Marker = a })

dt1Limit :: Lens' DescribeTapes (Maybe Integer)
dt1Limit = lens _dt1Limit (\s a -> s { _dt1Limit = a })

instance ToPath DescribeTapes

instance ToQuery DescribeTapes

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes

data DescribeTapesResponse = DescribeTapesResponse
    { _dtrsrsTapes :: [Tape]
    , _dtrsrsMarker :: Maybe Text
    } deriving (Show, Generic)

dtrsrsTapes :: Lens' DescribeTapesResponse [Tape]
dtrsrsTapes = lens _dtrsrsTapes (\s a -> s { _dtrsrsTapes = a })

dtrsrsMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtrsrsMarker = lens _dtrsrsMarker (\s a -> s { _dtrsrsMarker = a })

instance FromJSON DescribeTapesResponse

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq & dt1Marker ?~ x)
        <$> (rs ^. dtrsrsMarker)
