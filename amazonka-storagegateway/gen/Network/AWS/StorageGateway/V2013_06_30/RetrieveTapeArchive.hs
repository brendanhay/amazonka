{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
    (
    -- * Request
      RetrieveTapeArchive
    -- ** Request constructor
    , mkRetrieveTapeArchiveInput
    -- ** Request lenses
    , rtaiTapeARN
    , rtaiGatewayARN

    -- * Response
    , RetrieveTapeArchiveResponse
    -- ** Response lenses
    , rtaoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveTapeArchive' request.
mkRetrieveTapeArchiveInput :: Text -- ^ 'rtaiTapeARN'
                           -> Text -- ^ 'rtaiGatewayARN'
                           -> RetrieveTapeArchive
mkRetrieveTapeArchiveInput p1 p2 = RetrieveTapeArchive
    { _rtaiTapeARN = p1
    , _rtaiGatewayARN = p2
    }
{-# INLINE mkRetrieveTapeArchiveInput #-}

data RetrieveTapeArchive = RetrieveTapeArchive
    { _rtaiTapeARN :: Text
    , _rtaiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

rtaiTapeARN :: Lens' RetrieveTapeArchive (Text)
rtaiTapeARN = lens _rtaiTapeARN (\s a -> s { _rtaiTapeARN = a })
{-# INLINE rtaiTapeARN #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtaiGatewayARN :: Lens' RetrieveTapeArchive (Text)
rtaiGatewayARN = lens _rtaiGatewayARN (\s a -> s { _rtaiGatewayARN = a })
{-# INLINE rtaiGatewayARN #-}

instance ToPath RetrieveTapeArchive

instance ToQuery RetrieveTapeArchive

instance ToHeaders RetrieveTapeArchive

instance ToJSON RetrieveTapeArchive

newtype RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse
    { _rtaoTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtaoTapeARN :: Lens' RetrieveTapeArchiveResponse (Maybe Text)
rtaoTapeARN = lens _rtaoTapeARN (\s a -> s { _rtaoTapeARN = a })
{-# INLINE rtaoTapeARN #-}

instance FromJSON RetrieveTapeArchiveResponse

instance AWSRequest RetrieveTapeArchive where
    type Sv RetrieveTapeArchive = StorageGateway
    type Rs RetrieveTapeArchive = RetrieveTapeArchiveResponse

    request = get
    response _ = jsonResponse
