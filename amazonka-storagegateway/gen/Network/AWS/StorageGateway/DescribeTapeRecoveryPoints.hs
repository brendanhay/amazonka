{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway
    (
    -- * Request
      DescribeTapeRecoveryPoints
    -- ** Request constructor
    , mkDescribeTapeRecoveryPoints
    -- ** Request lenses
    , dtrpGatewayARN
    , dtrpMarker
    , dtrpLimit

    -- * Response
    , DescribeTapeRecoveryPointsResponse
    -- ** Response constructor
    , mkDescribeTapeRecoveryPointsResponse
    -- ** Response lenses
    , dtrprGatewayARN
    , dtrprTapeRecoveryPointInfos
    , dtrprMarker
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints
    { _dtrpGatewayARN :: !Text
    , _dtrpMarker :: !(Maybe Text)
    , _dtrpLimit :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeRecoveryPoints' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Limit ::@ @Maybe Integer@
--
mkDescribeTapeRecoveryPoints :: Text -- ^ 'dtrpGatewayARN'
                             -> DescribeTapeRecoveryPoints
mkDescribeTapeRecoveryPoints p1 = DescribeTapeRecoveryPoints
    { _dtrpGatewayARN = p1
    , _dtrpMarker = Nothing
    , _dtrpLimit = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrpGatewayARN :: Lens' DescribeTapeRecoveryPoints Text
dtrpGatewayARN = lens _dtrpGatewayARN (\s a -> s { _dtrpGatewayARN = a })

dtrpMarker :: Lens' DescribeTapeRecoveryPoints (Maybe Text)
dtrpMarker = lens _dtrpMarker (\s a -> s { _dtrpMarker = a })

dtrpLimit :: Lens' DescribeTapeRecoveryPoints (Maybe Integer)
dtrpLimit = lens _dtrpLimit (\s a -> s { _dtrpLimit = a })

instance ToPath DescribeTapeRecoveryPoints

instance ToQuery DescribeTapeRecoveryPoints

instance ToHeaders DescribeTapeRecoveryPoints

instance ToJSON DescribeTapeRecoveryPoints

data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { _dtrprGatewayARN :: !(Maybe Text)
    , _dtrprTapeRecoveryPointInfos :: [TapeRecoveryPointInfo]
    , _dtrprMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeRecoveryPointsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
-- * @TapeRecoveryPointInfos ::@ @[TapeRecoveryPointInfo]@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse
mkDescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { _dtrprGatewayARN = Nothing
    , _dtrprTapeRecoveryPointInfos = mempty
    , _dtrprMarker = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrprGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprGatewayARN = lens _dtrprGatewayARN (\s a -> s { _dtrprGatewayARN = a })

dtrprTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse [TapeRecoveryPointInfo]
dtrprTapeRecoveryPointInfos =
    lens _dtrprTapeRecoveryPointInfos
         (\s a -> s { _dtrprTapeRecoveryPointInfos = a })

dtrprMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprMarker = lens _dtrprMarker (\s a -> s { _dtrprMarker = a })

instance FromJSON DescribeTapeRecoveryPointsResponse

instance AWSRequest DescribeTapeRecoveryPoints where
    type Sv DescribeTapeRecoveryPoints = StorageGateway
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeRecoveryPoints where
    next rq rs = (\x -> rq & dtrpMarker ?~ x)
        <$> (rs ^. dtrprMarker)
