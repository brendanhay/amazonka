{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints
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
    -- ** Response lenses
    , dtrprsGatewayARN
    , dtrprsTapeRecoveryPointInfos
    , dtrprsMarker
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints
    { _dtrpGatewayARN :: Text
    , _dtrpMarker :: Maybe Text
    , _dtrpLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeRecoveryPoints' request.
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
    { _dtrprsGatewayARN :: Maybe Text
    , _dtrprsTapeRecoveryPointInfos :: [TapeRecoveryPointInfo]
    , _dtrprsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrprsGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprsGatewayARN =
    lens _dtrprsGatewayARN (\s a -> s { _dtrprsGatewayARN = a })

dtrprsTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse [TapeRecoveryPointInfo]
dtrprsTapeRecoveryPointInfos =
    lens _dtrprsTapeRecoveryPointInfos
         (\s a -> s { _dtrprsTapeRecoveryPointInfos = a })

dtrprsMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrprsMarker = lens _dtrprsMarker (\s a -> s { _dtrprsMarker = a })

instance FromJSON DescribeTapeRecoveryPointsResponse

instance AWSRequest DescribeTapeRecoveryPoints where
    type Sv DescribeTapeRecoveryPoints = StorageGateway
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeRecoveryPoints where
    next rq rs = (\x -> rq & dtrpMarker ?~ x)
        <$> (rs ^. dtrprsMarker)
