{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CancelArchival
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.CancelArchival
    (
    -- * Request
      CancelArchival
    -- ** Request constructor
    , cancelArchival
    -- ** Request lenses
    , caiGatewayARN
    , caiTapeARN

    -- * Response
    , CancelArchivalResponse
    -- ** Response lenses
    , caoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CancelArchival' request.
cancelArchival :: Text -- ^ 'caiGatewayARN'
               -> Text -- ^ 'caiTapeARN'
               -> CancelArchival
cancelArchival p1 p2 = CancelArchival
    { _caiGatewayARN = p1
    , _caiTapeARN = p2
    }
{-# INLINE cancelArchival #-}

data CancelArchival = CancelArchival
    { _caiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _caiTapeARN :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
caiGatewayARN :: Lens' CancelArchival (Text)
caiGatewayARN f x =
    f (_caiGatewayARN x)
        <&> \y -> x { _caiGatewayARN = y }
{-# INLINE caiGatewayARN #-}

caiTapeARN :: Lens' CancelArchival (Text)
caiTapeARN f x =
    f (_caiTapeARN x)
        <&> \y -> x { _caiTapeARN = y }
{-# INLINE caiTapeARN #-}

instance ToPath CancelArchival

instance ToQuery CancelArchival

instance ToHeaders CancelArchival

instance ToJSON CancelArchival

data CancelArchivalResponse = CancelArchivalResponse
    { _caoTapeARN :: Maybe Text
    } deriving (Show, Generic)

caoTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
caoTapeARN f x =
    f (_caoTapeARN x)
        <&> \y -> x { _caoTapeARN = y }
{-# INLINE caoTapeARN #-}

instance FromJSON CancelArchivalResponse

instance AWSRequest CancelArchival where
    type Sv CancelArchival = StorageGateway
    type Rs CancelArchival = CancelArchivalResponse

    request = get
    response _ = jsonResponse
