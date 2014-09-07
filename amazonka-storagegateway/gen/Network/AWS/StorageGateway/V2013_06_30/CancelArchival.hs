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
    , mkCancelArchival
    -- ** Request lenses
    , caGatewayARN
    , caTapeARN

    -- * Response
    , CancelArchivalResponse
    -- ** Response lenses
    , carsTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data CancelArchival = CancelArchival
    { _caGatewayARN :: Text
    , _caTapeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelArchival' request.
mkCancelArchival :: Text -- ^ 'caGatewayARN'
                 -> Text -- ^ 'caTapeARN'
                 -> CancelArchival
mkCancelArchival p1 p2 = CancelArchival
    { _caGatewayARN = p1
    , _caTapeARN = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
caGatewayARN :: Lens' CancelArchival Text
caGatewayARN = lens _caGatewayARN (\s a -> s { _caGatewayARN = a })

caTapeARN :: Lens' CancelArchival Text
caTapeARN = lens _caTapeARN (\s a -> s { _caTapeARN = a })

instance ToPath CancelArchival

instance ToQuery CancelArchival

instance ToHeaders CancelArchival

instance ToJSON CancelArchival

newtype CancelArchivalResponse = CancelArchivalResponse
    { _carsTapeARN :: Maybe Text
    } deriving (Show, Generic)

carsTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
carsTapeARN = lens _carsTapeARN (\s a -> s { _carsTapeARN = a })

instance FromJSON CancelArchivalResponse

instance AWSRequest CancelArchival where
    type Sv CancelArchival = StorageGateway
    type Rs CancelArchival = CancelArchivalResponse

    request = get
    response _ = jsonResponse
