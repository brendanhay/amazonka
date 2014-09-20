{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.DescribeTapes
    (
    -- * Request
      DescribeTapes
    -- ** Request constructor
    , describeTapes
    -- ** Request lenses
    , dt1GatewayARN
    , dt1TapeARNs
    , dt1Marker
    , dt1Limit

    -- * Response
    , DescribeTapesResponse
    -- ** Response constructor
    , describeTapesResponse
    -- ** Response lenses
    , dtrrTapes
    , dtrrMarker
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeTapes = DescribeTapes
    { _dt1GatewayARN :: Text
    , _dt1TapeARNs :: [Text]
    , _dt1Marker :: Maybe Text
    , _dt1Limit :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @TapeARNs ::@ @[Text]@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Limit ::@ @Maybe Integer@
--
describeTapes :: Text -- ^ 'dt1GatewayARN'
              -> DescribeTapes
describeTapes p1 = DescribeTapes
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
    { _dtrrTapes :: [Tape]
    , _dtrrMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Tapes ::@ @[Tape]@
--
-- * @Marker ::@ @Maybe Text@
--
describeTapesResponse :: DescribeTapesResponse
describeTapesResponse = DescribeTapesResponse
    { _dtrrTapes = mempty
    , _dtrrMarker = Nothing
    }

dtrrTapes :: Lens' DescribeTapesResponse [Tape]
dtrrTapes = lens _dtrrTapes (\s a -> s { _dtrrTapes = a })

dtrrMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtrrMarker = lens _dtrrMarker (\s a -> s { _dtrrMarker = a })

instance FromJSON DescribeTapesResponse

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq & dt1Marker ?~ x)
        <$> (rs ^. dtrrMarker)
