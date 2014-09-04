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
    , mkRetrieveTapeRecoveryPointInput
    -- ** Request lenses
    , rtrpiTapeARN
    , rtrpiGatewayARN

    -- * Response
    , RetrieveTapeRecoveryPointResponse
    -- ** Response lenses
    , rtrpoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveTapeRecoveryPoint' request.
mkRetrieveTapeRecoveryPointInput :: Text -- ^ 'rtrpiTapeARN'
                                 -> Text -- ^ 'rtrpiGatewayARN'
                                 -> RetrieveTapeRecoveryPoint
mkRetrieveTapeRecoveryPointInput p1 p2 = RetrieveTapeRecoveryPoint
    { _rtrpiTapeARN = p1
    , _rtrpiGatewayARN = p2
    }
{-# INLINE mkRetrieveTapeRecoveryPointInput #-}

data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint
    { _rtrpiTapeARN :: Text
    , _rtrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

rtrpiTapeARN :: Lens' RetrieveTapeRecoveryPoint (Text)
rtrpiTapeARN = lens _rtrpiTapeARN (\s a -> s { _rtrpiTapeARN = a })
{-# INLINE rtrpiTapeARN #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtrpiGatewayARN :: Lens' RetrieveTapeRecoveryPoint (Text)
rtrpiGatewayARN = lens _rtrpiGatewayARN (\s a -> s { _rtrpiGatewayARN = a })
{-# INLINE rtrpiGatewayARN #-}

instance ToPath RetrieveTapeRecoveryPoint

instance ToQuery RetrieveTapeRecoveryPoint

instance ToHeaders RetrieveTapeRecoveryPoint

instance ToJSON RetrieveTapeRecoveryPoint

newtype RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse
    { _rtrpoTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtrpoTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrpoTapeARN = lens _rtrpoTapeARN (\s a -> s { _rtrpoTapeARN = a })
{-# INLINE rtrpoTapeARN #-}

instance FromJSON RetrieveTapeRecoveryPointResponse

instance AWSRequest RetrieveTapeRecoveryPoint where
    type Sv RetrieveTapeRecoveryPoint = StorageGateway
    type Rs RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPointResponse

    request = get
    response _ = jsonResponse
