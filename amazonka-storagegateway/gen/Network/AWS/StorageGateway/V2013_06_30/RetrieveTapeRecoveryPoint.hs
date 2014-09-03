{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint
    (
    -- * Request
      RetrieveTapeRecoveryPoint
    -- ** Request constructor
    , retrieveTapeRecoveryPoint
    -- ** Request lenses
    , rtrpiGatewayARN
    , rtrpiTapeARN

    -- * Response
    , RetrieveTapeRecoveryPointResponse
    -- ** Response lenses
    , rtrpoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'RetrieveTapeRecoveryPoint' request.
retrieveTapeRecoveryPoint :: Text -- ^ 'rtrpiGatewayARN'
                          -> Text -- ^ 'rtrpiTapeARN'
                          -> RetrieveTapeRecoveryPoint
retrieveTapeRecoveryPoint p1 p2 = RetrieveTapeRecoveryPoint
    { _rtrpiGatewayARN = p1
    , _rtrpiTapeARN = p2
    }

data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint
    { _rtrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _rtrpiTapeARN :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtrpiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> RetrieveTapeRecoveryPoint
    -> f RetrieveTapeRecoveryPoint
rtrpiGatewayARN f x =
    (\y -> x { _rtrpiGatewayARN = y })
       <$> f (_rtrpiGatewayARN x)
{-# INLINE rtrpiGatewayARN #-}

rtrpiTapeARN
    :: Functor f
    => (Text
    -> f (Text))
    -> RetrieveTapeRecoveryPoint
    -> f RetrieveTapeRecoveryPoint
rtrpiTapeARN f x =
    (\y -> x { _rtrpiTapeARN = y })
       <$> f (_rtrpiTapeARN x)
{-# INLINE rtrpiTapeARN #-}

instance ToPath RetrieveTapeRecoveryPoint

instance ToQuery RetrieveTapeRecoveryPoint

instance ToHeaders RetrieveTapeRecoveryPoint

instance ToJSON RetrieveTapeRecoveryPoint

data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse
    { _rtrpoTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtrpoTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RetrieveTapeRecoveryPointResponse
    -> f RetrieveTapeRecoveryPointResponse
rtrpoTapeARN f x =
    (\y -> x { _rtrpoTapeARN = y })
       <$> f (_rtrpoTapeARN x)
{-# INLINE rtrpoTapeARN #-}

instance FromJSON RetrieveTapeRecoveryPointResponse

instance AWSRequest RetrieveTapeRecoveryPoint where
    type Sv RetrieveTapeRecoveryPoint = StorageGateway
    type Rs RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPointResponse

    request = get
    response _ = jsonResponse
