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
    , mkRetrieveTapeRecoveryPoint
    -- ** Request lenses
    , rtrpTapeARN
    , rtrpGatewayARN

    -- * Response
    , RetrieveTapeRecoveryPointResponse
    -- ** Response lenses
    , rtrprsTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint
    { _rtrpTapeARN :: Text
    , _rtrpGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveTapeRecoveryPoint' request.
mkRetrieveTapeRecoveryPoint :: Text -- ^ 'rtrpTapeARN'
                            -> Text -- ^ 'rtrpGatewayARN'
                            -> RetrieveTapeRecoveryPoint
mkRetrieveTapeRecoveryPoint p1 p2 = RetrieveTapeRecoveryPoint
    { _rtrpTapeARN = p1
    , _rtrpGatewayARN = p2
    }
{-# INLINE mkRetrieveTapeRecoveryPoint #-}

rtrpTapeARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrpTapeARN = lens _rtrpTapeARN (\s a -> s { _rtrpTapeARN = a })
{-# INLINE rtrpTapeARN #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtrpGatewayARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrpGatewayARN = lens _rtrpGatewayARN (\s a -> s { _rtrpGatewayARN = a })
{-# INLINE rtrpGatewayARN #-}

instance ToPath RetrieveTapeRecoveryPoint

instance ToQuery RetrieveTapeRecoveryPoint

instance ToHeaders RetrieveTapeRecoveryPoint

instance ToJSON RetrieveTapeRecoveryPoint

newtype RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse
    { _rtrprsTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtrprsTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrprsTapeARN = lens _rtrprsTapeARN (\s a -> s { _rtrprsTapeARN = a })
{-# INLINE rtrprsTapeARN #-}

instance FromJSON RetrieveTapeRecoveryPointResponse

instance AWSRequest RetrieveTapeRecoveryPoint where
    type Sv RetrieveTapeRecoveryPoint = StorageGateway
    type Rs RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPointResponse

    request = get
    response _ = jsonResponse
