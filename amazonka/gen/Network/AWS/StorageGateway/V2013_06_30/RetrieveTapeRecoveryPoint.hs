{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint
    { _rtrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _rtrpiTapeARN :: Text
    } deriving (Show, Generic)

makeLenses ''RetrieveTapeRecoveryPoint

instance ToPath RetrieveTapeRecoveryPoint

instance ToQuery RetrieveTapeRecoveryPoint

instance ToHeaders RetrieveTapeRecoveryPoint

instance ToJSON RetrieveTapeRecoveryPoint

data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse
    { _rtrpoTapeARN :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''RetrieveTapeRecoveryPointResponse

instance FromJSON RetrieveTapeRecoveryPointResponse

instance AWSRequest RetrieveTapeRecoveryPoint where
    type Sv RetrieveTapeRecoveryPoint = StorageGateway
    type Rs RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPointResponse

    request = get
    response _ = undefined
