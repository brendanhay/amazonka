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
dtrpiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeTapeRecoveryPoints
    -> f DescribeTapeRecoveryPoints
dtrpiGatewayARN f x =
    (\y -> x { _dtrpiGatewayARN = y })
       <$> f (_dtrpiGatewayARN x)
{-# INLINE dtrpiGatewayARN #-}

dtrpiMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapeRecoveryPoints
    -> f DescribeTapeRecoveryPoints
dtrpiMarker f x =
    (\y -> x { _dtrpiMarker = y })
       <$> f (_dtrpiMarker x)
{-# INLINE dtrpiMarker #-}

dtrpiLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeTapeRecoveryPoints
    -> f DescribeTapeRecoveryPoints
dtrpiLimit f x =
    (\y -> x { _dtrpiLimit = y })
       <$> f (_dtrpiLimit x)
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
dtrpoGatewayARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapeRecoveryPointsResponse
    -> f DescribeTapeRecoveryPointsResponse
dtrpoGatewayARN f x =
    (\y -> x { _dtrpoGatewayARN = y })
       <$> f (_dtrpoGatewayARN x)
{-# INLINE dtrpoGatewayARN #-}

dtrpoMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapeRecoveryPointsResponse
    -> f DescribeTapeRecoveryPointsResponse
dtrpoMarker f x =
    (\y -> x { _dtrpoMarker = y })
       <$> f (_dtrpoMarker x)
{-# INLINE dtrpoMarker #-}

dtrpoTapeRecoveryPointInfos
    :: Functor f
    => ([TapeRecoveryPointInfo]
    -> f ([TapeRecoveryPointInfo]))
    -> DescribeTapeRecoveryPointsResponse
    -> f DescribeTapeRecoveryPointsResponse
dtrpoTapeRecoveryPointInfos f x =
    (\y -> x { _dtrpoTapeRecoveryPointInfos = y })
       <$> f (_dtrpoTapeRecoveryPointInfos x)
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
