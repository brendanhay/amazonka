{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.CreateTapes
    (
    -- * Request
      CreateTapes
    -- ** Request constructor
    , createTapes
    -- ** Request lenses
    , ctiClientToken
    , ctiGatewayARN
    , ctiNumTapesToCreate
    , ctiTapeBarcodePrefix
    , ctiTapeSizeInBytes

    -- * Response
    , CreateTapesResponse
    -- ** Response lenses
    , ctoTapeARNs
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateTapes' request.
createTapes :: Text -- ^ 'ctiClientToken'
            -> Text -- ^ 'ctiGatewayARN'
            -> Integer -- ^ 'ctiNumTapesToCreate'
            -> Text -- ^ 'ctiTapeBarcodePrefix'
            -> Integer -- ^ 'ctiTapeSizeInBytes'
            -> CreateTapes
createTapes p1 p2 p3 p4 p5 = CreateTapes
    { _ctiClientToken = p1
    , _ctiGatewayARN = p2
    , _ctiNumTapesToCreate = p3
    , _ctiTapeBarcodePrefix = p4
    , _ctiTapeSizeInBytes = p5
    }

data CreateTapes = CreateTapes
    { _ctiClientToken :: Text
    , _ctiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ctiNumTapesToCreate :: Integer
    , _ctiTapeBarcodePrefix :: Text
    , _ctiTapeSizeInBytes :: Integer
    } deriving (Show, Generic)

ctiClientToken
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTapes
    -> f CreateTapes
ctiClientToken f x =
    (\y -> x { _ctiClientToken = y })
       <$> f (_ctiClientToken x)
{-# INLINE ctiClientToken #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ctiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTapes
    -> f CreateTapes
ctiGatewayARN f x =
    (\y -> x { _ctiGatewayARN = y })
       <$> f (_ctiGatewayARN x)
{-# INLINE ctiGatewayARN #-}

ctiNumTapesToCreate
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateTapes
    -> f CreateTapes
ctiNumTapesToCreate f x =
    (\y -> x { _ctiNumTapesToCreate = y })
       <$> f (_ctiNumTapesToCreate x)
{-# INLINE ctiNumTapesToCreate #-}

ctiTapeBarcodePrefix
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTapes
    -> f CreateTapes
ctiTapeBarcodePrefix f x =
    (\y -> x { _ctiTapeBarcodePrefix = y })
       <$> f (_ctiTapeBarcodePrefix x)
{-# INLINE ctiTapeBarcodePrefix #-}

ctiTapeSizeInBytes
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateTapes
    -> f CreateTapes
ctiTapeSizeInBytes f x =
    (\y -> x { _ctiTapeSizeInBytes = y })
       <$> f (_ctiTapeSizeInBytes x)
{-# INLINE ctiTapeSizeInBytes #-}

instance ToPath CreateTapes

instance ToQuery CreateTapes

instance ToHeaders CreateTapes

instance ToJSON CreateTapes

data CreateTapesResponse = CreateTapesResponse
    { _ctoTapeARNs :: [Text]
    } deriving (Show, Generic)

ctoTapeARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateTapesResponse
    -> f CreateTapesResponse
ctoTapeARNs f x =
    (\y -> x { _ctoTapeARNs = y })
       <$> f (_ctoTapeARNs x)
{-# INLINE ctoTapeARNs #-}

instance FromJSON CreateTapesResponse

instance AWSRequest CreateTapes where
    type Sv CreateTapes = StorageGateway
    type Rs CreateTapes = CreateTapesResponse

    request = get
    response _ = jsonResponse
