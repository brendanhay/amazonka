{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the user's sending statistics. The result is a list of data points,
-- representing the last two weeks of sending activity.
--
-- Each data point in the list contains statistics for a 15-minute interval.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetSendStatistics.html>
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
import qualified GHC.Exts

data GetSendStatistics = GetSendStatistics
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'GetSendStatistics' constructor.
getSendStatistics :: GetSendStatistics
getSendStatistics = GetSendStatistics

newtype GetSendStatisticsResponse = GetSendStatisticsResponse
    { _gssrSendDataPoints :: List "member" SendDataPoint
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList GetSendStatisticsResponse where
    type Item GetSendStatisticsResponse = SendDataPoint

    fromList = GetSendStatisticsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _gssrSendDataPoints

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
        . _List

instance ToPath GetSendStatistics where
    toPath = const "/"

instance ToQuery GetSendStatistics where
    toQuery = const mempty

instance ToHeaders GetSendStatistics

instance AWSRequest GetSendStatistics where
    type Sv GetSendStatistics = SES
    type Rs GetSendStatistics = GetSendStatisticsResponse

    request  = post "GetSendStatistics"
    response = xmlResponse

instance FromXML GetSendStatisticsResponse where
    parseXML = withElement "GetSendStatisticsResult" $ \x -> GetSendStatisticsResponse
        <$> x .@? "SendDataPoints" .!@ mempty
