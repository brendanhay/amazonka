{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.CreateTapes where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

data CreateTapes = CreateTapes
    { _ctiClientToken :: Text
    , _ctiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ctiNumTapesToCreate :: Integer
    , _ctiTapeBarcodePrefix :: Text
    , _ctiTapeSizeInBytes :: Integer
    } deriving (Generic)

makeLenses ''CreateTapes

instance ToPath CreateTapes

instance ToQuery CreateTapes

instance ToHeaders CreateTapes

instance ToJSON CreateTapes

data CreateTapesResponse = CreateTapesResponse
    { _ctoTapeARNs :: [Text]
    } deriving (Generic)

makeLenses ''CreateTapesResponse

instance FromJSON CreateTapesResponse

instance AWSRequest CreateTapes where
    type Sv CreateTapes = StorageGateway
    type Rs CreateTapes = CreateTapesResponse

    request = get
    response _ = jsonResponse
