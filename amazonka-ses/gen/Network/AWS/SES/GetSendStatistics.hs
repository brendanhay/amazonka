{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the user's sending statistics. The result is a list of data points,
-- representing the last two weeks of sending activity. Each data point in the
-- list contains statistics for a 15-minute interval. This action is throttled
-- at one request per second.
module Network.AWS.SES.GetSendStatistics
    (
    -- * Request
      GetSendStatistics
    -- ** Request constructor
    , getSendStatistics

    -- * Response
    , GetSendStatisticsResponse
    -- ** Response constructor
    , getSendStatisticsResponse
    -- ** Response lenses
    , gssrSendDataPoints
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

data GetSendStatistics = GetSendStatistics
    deriving (Eq, Ord, Show, Generic)

-- | 'GetSendStatistics' constructor.
getSendStatistics :: GetSendStatistics
getSendStatistics = GetSendStatistics
instance ToQuery GetSendStatistics

instance ToPath GetSendStatistics where
    toPath = const "/"

newtype GetSendStatisticsResponse = GetSendStatisticsResponse
    { _gssrSendDataPoints :: [SendDataPoint]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

-- | 'GetSendStatisticsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gssrSendDataPoints' @::@ ['SendDataPoint']
--
getSendStatisticsResponse :: GetSendStatisticsResponse
getSendStatisticsResponse = GetSendStatisticsResponse
    { _gssrSendDataPoints = mempty
    }

-- | A list of data points, each of which represents 15 minutes of activity.
gssrSendDataPoints :: Lens' GetSendStatisticsResponse [SendDataPoint]
gssrSendDataPoints =
    lens _gssrSendDataPoints (\s a -> s { _gssrSendDataPoints = a })

instance FromXML GetSendStatisticsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetSendStatisticsResponse"

instance AWSRequest GetSendStatistics where
    type Sv GetSendStatistics = SES
    type Rs GetSendStatistics = GetSendStatisticsResponse

    request  = post "GetSendStatistics"
    response = xmlResponse $ \h x -> GetSendStatisticsResponse
        <$> x %| "SendDataPoints"
