{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DeleteTape
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DeleteTape
    (
    -- * Request
      DeleteTape
    -- ** Request constructor
    , mkDeleteTape
    -- ** Request lenses
    , dtGatewayARN
    , dtTapeARN

    -- * Response
    , DeleteTapeResponse
    -- ** Response lenses
    , dtrTapeARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeleteTape = DeleteTape
    { _dtGatewayARN :: Text
    , _dtTapeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTape' request.
mkDeleteTape :: Text -- ^ 'dtGatewayARN'
             -> Text -- ^ 'dtTapeARN'
             -> DeleteTape
mkDeleteTape p1 p2 = DeleteTape
    { _dtGatewayARN = p1
    , _dtTapeARN = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dtGatewayARN :: Lens' DeleteTape Text
dtGatewayARN = lens _dtGatewayARN (\s a -> s { _dtGatewayARN = a })

dtTapeARN :: Lens' DeleteTape Text
dtTapeARN = lens _dtTapeARN (\s a -> s { _dtTapeARN = a })

instance ToPath DeleteTape

instance ToQuery DeleteTape

instance ToHeaders DeleteTape

instance ToJSON DeleteTape

newtype DeleteTapeResponse = DeleteTapeResponse
    { _dtrTapeARN :: Maybe Text
    } deriving (Show, Generic)

dtrTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrTapeARN = lens _dtrTapeARN (\s a -> s { _dtrTapeARN = a })

instance FromJSON DeleteTapeResponse

instance AWSRequest DeleteTape where
    type Sv DeleteTape = StorageGateway
    type Rs DeleteTape = DeleteTapeResponse

    request = get
    response _ = jsonResponse
