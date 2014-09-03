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
    , describeTapes
    -- ** Request lenses
    , dtjGatewayARN
    , dtjMarker
    , dtjLimit
    , dtjTapeARNs

    -- * Response
    , DescribeTapesResponse
    -- ** Response lenses
    , dtpMarker
    , dtpTapes
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeTapes' request.
describeTapes :: Text -- ^ 'dtjGatewayARN'
              -> DescribeTapes
describeTapes p1 = DescribeTapes
    { _dtjGatewayARN = p1
    , _dtjMarker = Nothing
    , _dtjLimit = Nothing
    , _dtjTapeARNs = mempty
    }

data DescribeTapes = DescribeTapes
    { _dtjGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dtjMarker :: Maybe Text
    , _dtjLimit :: Maybe Integer
    , _dtjTapeARNs :: [Text]
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtjGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeTapes
    -> f DescribeTapes
dtjGatewayARN f x =
    (\y -> x { _dtjGatewayARN = y })
       <$> f (_dtjGatewayARN x)
{-# INLINE dtjGatewayARN #-}

dtjMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapes
    -> f DescribeTapes
dtjMarker f x =
    (\y -> x { _dtjMarker = y })
       <$> f (_dtjMarker x)
{-# INLINE dtjMarker #-}

dtjLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeTapes
    -> f DescribeTapes
dtjLimit f x =
    (\y -> x { _dtjLimit = y })
       <$> f (_dtjLimit x)
{-# INLINE dtjLimit #-}

dtjTapeARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeTapes
    -> f DescribeTapes
dtjTapeARNs f x =
    (\y -> x { _dtjTapeARNs = y })
       <$> f (_dtjTapeARNs x)
{-# INLINE dtjTapeARNs #-}

instance ToPath DescribeTapes

instance ToQuery DescribeTapes

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes

data DescribeTapesResponse = DescribeTapesResponse
    { _dtpMarker :: Maybe Text
    , _dtpTapes :: [Tape]
    } deriving (Show, Generic)

dtpMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapesResponse
    -> f DescribeTapesResponse
dtpMarker f x =
    (\y -> x { _dtpMarker = y })
       <$> f (_dtpMarker x)
{-# INLINE dtpMarker #-}

dtpTapes
    :: Functor f
    => ([Tape]
    -> f ([Tape]))
    -> DescribeTapesResponse
    -> f DescribeTapesResponse
dtpTapes f x =
    (\y -> x { _dtpTapes = y })
       <$> f (_dtpTapes x)
{-# INLINE dtpTapes #-}

instance FromJSON DescribeTapesResponse

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq { _dtjMarker = Just x })
        <$> (_dtpMarker rs)
