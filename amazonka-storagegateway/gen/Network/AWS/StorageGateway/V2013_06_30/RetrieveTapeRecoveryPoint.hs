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
{-# INLINE retrieveTapeRecoveryPoint #-}

data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint
    { _rtrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _rtrpiTapeARN :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtrpiGatewayARN :: Lens' RetrieveTapeRecoveryPoint (Text)
rtrpiGatewayARN f x =
    f (_rtrpiGatewayARN x)
        <&> \y -> x { _rtrpiGatewayARN = y }
{-# INLINE rtrpiGatewayARN #-}

rtrpiTapeARN :: Lens' RetrieveTapeRecoveryPoint (Text)
rtrpiTapeARN f x =
    f (_rtrpiTapeARN x)
        <&> \y -> x { _rtrpiTapeARN = y }
{-# INLINE rtrpiTapeARN #-}

instance ToPath RetrieveTapeRecoveryPoint

instance ToQuery RetrieveTapeRecoveryPoint

instance ToHeaders RetrieveTapeRecoveryPoint

instance ToJSON RetrieveTapeRecoveryPoint

data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse
    { _rtrpoTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtrpoTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrpoTapeARN f x =
    f (_rtrpoTapeARN x)
        <&> \y -> x { _rtrpoTapeARN = y }
{-# INLINE rtrpoTapeARN #-}

instance FromJSON RetrieveTapeRecoveryPointResponse

instance AWSRequest RetrieveTapeRecoveryPoint where
    type Sv RetrieveTapeRecoveryPoint = StorageGateway
    type Rs RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPointResponse

    request = get
    response _ = jsonResponse
