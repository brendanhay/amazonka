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
    , mkDescribeTapeRecoveryPointsInput
    -- ** Request lenses
    , dtrpiGatewayARN
    , dtrpiMarker
    , dtrpiLimit

    -- * Response
    , DescribeTapeRecoveryPointsResponse
    -- ** Response lenses
    , dtrpoGatewayARN
    , dtrpoTapeRecoveryPointInfos
    , dtrpoMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeRecoveryPoints' request.
mkDescribeTapeRecoveryPointsInput :: Text -- ^ 'dtrpiGatewayARN'
                                  -> DescribeTapeRecoveryPoints
mkDescribeTapeRecoveryPointsInput p1 = DescribeTapeRecoveryPoints
    { _dtrpiGatewayARN = p1
    , _dtrpiMarker = Nothing
    , _dtrpiLimit = Nothing
    }
{-# INLINE mkDescribeTapeRecoveryPointsInput #-}

data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints
    { _dtrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dtrpiMarker :: Maybe Text
    , _dtrpiLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrpiGatewayARN :: Lens' DescribeTapeRecoveryPoints (Text)
dtrpiGatewayARN = lens _dtrpiGatewayARN (\s a -> s { _dtrpiGatewayARN = a })
{-# INLINE dtrpiGatewayARN #-}

dtrpiMarker :: Lens' DescribeTapeRecoveryPoints (Maybe Text)
dtrpiMarker = lens _dtrpiMarker (\s a -> s { _dtrpiMarker = a })
{-# INLINE dtrpiMarker #-}

dtrpiLimit :: Lens' DescribeTapeRecoveryPoints (Maybe Integer)
dtrpiLimit = lens _dtrpiLimit (\s a -> s { _dtrpiLimit = a })
{-# INLINE dtrpiLimit #-}

instance ToPath DescribeTapeRecoveryPoints

instance ToQuery DescribeTapeRecoveryPoints

instance ToHeaders DescribeTapeRecoveryPoints

instance ToJSON DescribeTapeRecoveryPoints

data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { _dtrpoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dtrpoTapeRecoveryPointInfos :: [TapeRecoveryPointInfo]
    , _dtrpoMarker :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrpoGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrpoGatewayARN = lens _dtrpoGatewayARN (\s a -> s { _dtrpoGatewayARN = a })
{-# INLINE dtrpoGatewayARN #-}

dtrpoTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse ([TapeRecoveryPointInfo])
dtrpoTapeRecoveryPointInfos = lens _dtrpoTapeRecoveryPointInfos (\s a -> s { _dtrpoTapeRecoveryPointInfos = a })
{-# INLINE dtrpoTapeRecoveryPointInfos #-}

dtrpoMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrpoMarker = lens _dtrpoMarker (\s a -> s { _dtrpoMarker = a })
{-# INLINE dtrpoMarker #-}

instance FromJSON DescribeTapeRecoveryPointsResponse

instance AWSRequest DescribeTapeRecoveryPoints where
    type Sv DescribeTapeRecoveryPoints = StorageGateway
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeRecoveryPoints where
    next rq rs = (\x -> rq { _dtrpiMarker = Just x })
        <$> (_dtrpoMarker rs)
