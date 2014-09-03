{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval
    (
    -- * Request
      CancelRetrieval
    -- ** Request constructor
    , cancelRetrieval
    -- ** Request lenses
    , criGatewayARN
    , criTapeARN

    -- * Response
    , CancelRetrievalResponse
    -- ** Response lenses
    , croTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CancelRetrieval' request.
cancelRetrieval :: Text -- ^ 'criGatewayARN'
                -> Text -- ^ 'criTapeARN'
                -> CancelRetrieval
cancelRetrieval p1 p2 = CancelRetrieval
    { _criGatewayARN = p1
    , _criTapeARN = p2
    }

data CancelRetrieval = CancelRetrieval
    { _criGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _criTapeARN :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
criGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelRetrieval
    -> f CancelRetrieval
criGatewayARN f x =
    (\y -> x { _criGatewayARN = y })
       <$> f (_criGatewayARN x)
{-# INLINE criGatewayARN #-}

criTapeARN
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelRetrieval
    -> f CancelRetrieval
criTapeARN f x =
    (\y -> x { _criTapeARN = y })
       <$> f (_criTapeARN x)
{-# INLINE criTapeARN #-}

instance ToPath CancelRetrieval

instance ToQuery CancelRetrieval

instance ToHeaders CancelRetrieval

instance ToJSON CancelRetrieval

data CancelRetrievalResponse = CancelRetrievalResponse
    { _croTapeARN :: Maybe Text
    } deriving (Show, Generic)

croTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CancelRetrievalResponse
    -> f CancelRetrievalResponse
croTapeARN f x =
    (\y -> x { _croTapeARN = y })
       <$> f (_croTapeARN x)
{-# INLINE croTapeARN #-}

instance FromJSON CancelRetrievalResponse

instance AWSRequest CancelRetrieval where
    type Sv CancelRetrieval = StorageGateway
    type Rs CancelRetrieval = CancelRetrievalResponse

    request = get
    response _ = jsonResponse
