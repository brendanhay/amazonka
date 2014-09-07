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
    , mkRetrieveTapeArchive
    -- ** Request lenses
    , rtaTapeARN
    , rtaGatewayARN

    -- * Response
    , RetrieveTapeArchiveResponse
    -- ** Response lenses
    , rtarsTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data RetrieveTapeArchive = RetrieveTapeArchive
    { _rtaTapeARN :: Text
    , _rtaGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveTapeArchive' request.
mkRetrieveTapeArchive :: Text -- ^ 'rtaTapeARN'
                      -> Text -- ^ 'rtaGatewayARN'
                      -> RetrieveTapeArchive
mkRetrieveTapeArchive p1 p2 = RetrieveTapeArchive
    { _rtaTapeARN = p1
    , _rtaGatewayARN = p2
    }

rtaTapeARN :: Lens' RetrieveTapeArchive Text
rtaTapeARN = lens _rtaTapeARN (\s a -> s { _rtaTapeARN = a })

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtaGatewayARN :: Lens' RetrieveTapeArchive Text
rtaGatewayARN = lens _rtaGatewayARN (\s a -> s { _rtaGatewayARN = a })

instance ToPath RetrieveTapeArchive

instance ToQuery RetrieveTapeArchive

instance ToHeaders RetrieveTapeArchive

instance ToJSON RetrieveTapeArchive

newtype RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse
    { _rtarsTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtarsTapeARN :: Lens' RetrieveTapeArchiveResponse (Maybe Text)
rtarsTapeARN = lens _rtarsTapeARN (\s a -> s { _rtarsTapeARN = a })

instance FromJSON RetrieveTapeArchiveResponse

instance AWSRequest RetrieveTapeArchive where
    type Sv RetrieveTapeArchive = StorageGateway
    type Rs RetrieveTapeArchive = RetrieveTapeArchiveResponse

    request = get
    response _ = jsonResponse
