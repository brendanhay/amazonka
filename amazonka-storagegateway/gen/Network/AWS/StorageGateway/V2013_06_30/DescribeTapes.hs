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
    , mkDescribeTapesInput
    -- ** Request lenses
    , dtjGatewayARN
    , dtjTapeARNs
    , dtjMarker
    , dtjLimit

    -- * Response
    , DescribeTapesResponse
    -- ** Response lenses
    , dtpTapes
    , dtpMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapes' request.
mkDescribeTapesInput :: Text -- ^ 'dtjGatewayARN'
                     -> DescribeTapes
mkDescribeTapesInput p1 = DescribeTapes
    { _dtjGatewayARN = p1
    , _dtjTapeARNs = mempty
    , _dtjMarker = Nothing
    , _dtjLimit = Nothing
    }
{-# INLINE mkDescribeTapesInput #-}

data DescribeTapes = DescribeTapes
    { _dtjGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dtjTapeARNs :: [Text]
    , _dtjMarker :: Maybe Text
    , _dtjLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtjGatewayARN :: Lens' DescribeTapes (Text)
dtjGatewayARN = lens _dtjGatewayARN (\s a -> s { _dtjGatewayARN = a })
{-# INLINE dtjGatewayARN #-}

dtjTapeARNs :: Lens' DescribeTapes ([Text])
dtjTapeARNs = lens _dtjTapeARNs (\s a -> s { _dtjTapeARNs = a })
{-# INLINE dtjTapeARNs #-}

dtjMarker :: Lens' DescribeTapes (Maybe Text)
dtjMarker = lens _dtjMarker (\s a -> s { _dtjMarker = a })
{-# INLINE dtjMarker #-}

dtjLimit :: Lens' DescribeTapes (Maybe Integer)
dtjLimit = lens _dtjLimit (\s a -> s { _dtjLimit = a })
{-# INLINE dtjLimit #-}

instance ToPath DescribeTapes

instance ToQuery DescribeTapes

instance ToHeaders DescribeTapes

instance ToJSON DescribeTapes

data DescribeTapesResponse = DescribeTapesResponse
    { _dtpTapes :: [Tape]
    , _dtpMarker :: Maybe Text
    } deriving (Show, Generic)

dtpTapes :: Lens' DescribeTapesResponse ([Tape])
dtpTapes = lens _dtpTapes (\s a -> s { _dtpTapes = a })
{-# INLINE dtpTapes #-}

dtpMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtpMarker = lens _dtpMarker (\s a -> s { _dtpMarker = a })
{-# INLINE dtpMarker #-}

instance FromJSON DescribeTapesResponse

instance AWSRequest DescribeTapes where
    type Sv DescribeTapes = StorageGateway
    type Rs DescribeTapes = DescribeTapesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapes where
    next rq rs = (\x -> rq { _dtjMarker = Just x })
        <$> (_dtpMarker rs)
