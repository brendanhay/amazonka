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
    , mkCancelRetrieval
    -- ** Request lenses
    , crGatewayARN
    , crTapeARN

    -- * Response
    , CancelRetrievalResponse
    -- ** Response lenses
    , crrsTapeARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CancelRetrieval = CancelRetrieval
    { _crGatewayARN :: Text
    , _crTapeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelRetrieval' request.
mkCancelRetrieval :: Text -- ^ 'crGatewayARN'
                  -> Text -- ^ 'crTapeARN'
                  -> CancelRetrieval
mkCancelRetrieval p1 p2 = CancelRetrieval
    { _crGatewayARN = p1
    , _crTapeARN = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
crGatewayARN :: Lens' CancelRetrieval Text
crGatewayARN = lens _crGatewayARN (\s a -> s { _crGatewayARN = a })

crTapeARN :: Lens' CancelRetrieval Text
crTapeARN = lens _crTapeARN (\s a -> s { _crTapeARN = a })

instance ToPath CancelRetrieval

instance ToQuery CancelRetrieval

instance ToHeaders CancelRetrieval

instance ToJSON CancelRetrieval

newtype CancelRetrievalResponse = CancelRetrievalResponse
    { _crrsTapeARN :: Maybe Text
    } deriving (Show, Generic)

crrsTapeARN :: Lens' CancelRetrievalResponse (Maybe Text)
crrsTapeARN = lens _crrsTapeARN (\s a -> s { _crrsTapeARN = a })

instance FromJSON CancelRetrievalResponse

instance AWSRequest CancelRetrieval where
    type Sv CancelRetrieval = StorageGateway
    type Rs CancelRetrieval = CancelRetrievalResponse

    request = get
    response _ = jsonResponse
