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
    , describeTapeRecoveryPoints
    -- ** Request lenses
    , dtrpiGatewayARN
    , dtrpiMarker
    , dtrpiLimit

    -- * Response
    , DescribeTapeRecoveryPointsResponse
    -- ** Response lenses
    , dtrpoGatewayARN
    , dtrpoMarker
    , dtrpoTapeRecoveryPointInfos
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeTapeRecoveryPoints' request.
describeTapeRecoveryPoints :: Text -- ^ 'dtrpiGatewayARN'
                           -> DescribeTapeRecoveryPoints
describeTapeRecoveryPoints p1 = DescribeTapeRecoveryPoints
    { _dtrpiGatewayARN = p1
    , _dtrpiMarker = Nothing
    , _dtrpiLimit = Nothing
    }
{-# INLINE describeTapeRecoveryPoints #-}

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
dtrpiGatewayARN f x =
    f (_dtrpiGatewayARN x)
        <&> \y -> x { _dtrpiGatewayARN = y }
{-# INLINE dtrpiGatewayARN #-}

dtrpiMarker :: Lens' DescribeTapeRecoveryPoints (Maybe Text)
dtrpiMarker f x =
    f (_dtrpiMarker x)
        <&> \y -> x { _dtrpiMarker = y }
{-# INLINE dtrpiMarker #-}

dtrpiLimit :: Lens' DescribeTapeRecoveryPoints (Maybe Integer)
dtrpiLimit f x =
    f (_dtrpiLimit x)
        <&> \y -> x { _dtrpiLimit = y }
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
    , _dtrpoMarker :: Maybe Text
    , _dtrpoTapeRecoveryPointInfos :: [TapeRecoveryPointInfo]
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtrpoGatewayARN :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrpoGatewayARN f x =
    f (_dtrpoGatewayARN x)
        <&> \y -> x { _dtrpoGatewayARN = y }
{-# INLINE dtrpoGatewayARN #-}

dtrpoMarker :: Lens' DescribeTapeRecoveryPointsResponse (Maybe Text)
dtrpoMarker f x =
    f (_dtrpoMarker x)
        <&> \y -> x { _dtrpoMarker = y }
{-# INLINE dtrpoMarker #-}

dtrpoTapeRecoveryPointInfos :: Lens' DescribeTapeRecoveryPointsResponse ([TapeRecoveryPointInfo])
dtrpoTapeRecoveryPointInfos f x =
    f (_dtrpoTapeRecoveryPointInfos x)
        <&> \y -> x { _dtrpoTapeRecoveryPointInfos = y }
{-# INLINE dtrpoTapeRecoveryPointInfos #-}

instance FromJSON DescribeTapeRecoveryPointsResponse

instance AWSRequest DescribeTapeRecoveryPoints where
    type Sv DescribeTapeRecoveryPoints = StorageGateway
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeRecoveryPoints where
    next rq rs = (\x -> rq { _dtrpiMarker = Just x })
        <$> (_dtrpoMarker rs)
