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
{-# INLINE describeTapes #-}

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
dtjGatewayARN :: Lens' DescribeTapes (Text)
dtjGatewayARN f x =
    f (_dtjGatewayARN x)
        <&> \y -> x { _dtjGatewayARN = y }
{-# INLINE dtjGatewayARN #-}

dtjMarker :: Lens' DescribeTapes (Maybe Text)
dtjMarker f x =
    f (_dtjMarker x)
        <&> \y -> x { _dtjMarker = y }
{-# INLINE dtjMarker #-}

dtjLimit :: Lens' DescribeTapes (Maybe Integer)
dtjLimit f x =
    f (_dtjLimit x)
        <&> \y -> x { _dtjLimit = y }
{-# INLINE dtjLimit #-}

dtjTapeARNs :: Lens' DescribeTapes ([Text])
dtjTapeARNs f x =
    f (_dtjTapeARNs x)
        <&> \y -> x { _dtjTapeARNs = y }
{-# INLINE dtjTapeARNs #-}

instance ToPath DescribeTapes

instance ToQuery DescribeTapes

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes

data DescribeTapesResponse = DescribeTapesResponse
    { _dtpMarker :: Maybe Text
    , _dtpTapes :: [Tape]
    } deriving (Show, Generic)

dtpMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtpMarker f x =
    f (_dtpMarker x)
        <&> \y -> x { _dtpMarker = y }
{-# INLINE dtpMarker #-}

dtpTapes :: Lens' DescribeTapesResponse ([Tape])
dtpTapes f x =
    f (_dtpTapes x)
        <&> \y -> x { _dtpTapes = y }
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
