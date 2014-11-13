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

-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
module Network.AWS.CloudWatch.DescribeAlarms
    (
    -- * Request
      DescribeAlarms
    -- ** Request constructor
    , describeAlarms
    -- ** Request lenses
    , daActionPrefix
    , daAlarmNamePrefix
    , daAlarmNames
    , daMaxRecords
    , daNextToken
    , daStateValue

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response constructor
    , describeAlarmsResponse
    -- ** Response lenses
    , darMetricAlarms
    , darNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data DescribeAlarms = DescribeAlarms
    { _daActionPrefix    :: Maybe Text
    , _daAlarmNamePrefix :: Maybe Text
    , _daAlarmNames      :: [Text]
    , _daMaxRecords      :: Maybe Natural
    , _daNextToken       :: Maybe Text
    , _daStateValue      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAlarms' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daActionPrefix' @::@ 'Maybe' 'Text'
--
-- * 'daAlarmNamePrefix' @::@ 'Maybe' 'Text'
--
-- * 'daAlarmNames' @::@ ['Text']
--
-- * 'daMaxRecords' @::@ 'Maybe' 'Natural'
--
-- * 'daNextToken' @::@ 'Maybe' 'Text'
--
-- * 'daStateValue' @::@ 'Maybe' 'Text'
--
describeAlarms :: DescribeAlarms
describeAlarms = DescribeAlarms
    { _daAlarmNames      = mempty
    , _daAlarmNamePrefix = Nothing
    , _daStateValue      = Nothing
    , _daActionPrefix    = Nothing
    , _daMaxRecords      = Nothing
    , _daNextToken       = Nothing
    }

-- | The action name prefix.
daActionPrefix :: Lens' DescribeAlarms (Maybe Text)
daActionPrefix = lens _daActionPrefix (\s a -> s { _daActionPrefix = a })

-- | The alarm name prefix. AlarmNames cannot be specified if this parameter
-- is specified.
daAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
daAlarmNamePrefix =
    lens _daAlarmNamePrefix (\s a -> s { _daAlarmNamePrefix = a })

-- | A list of alarm names to retrieve information for.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\s a -> s { _daAlarmNames = a })

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\s a -> s { _daMaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\s a -> s { _daNextToken = a })

-- | The state value to be used in matching alarms.
daStateValue :: Lens' DescribeAlarms (Maybe Text)
daStateValue = lens _daStateValue (\s a -> s { _daStateValue = a })

instance ToQuery DescribeAlarms

instance ToPath DescribeAlarms where
    toPath = const "/"

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { _darMetricAlarms :: [MetricAlarm]
    , _darNextToken    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAlarmsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darMetricAlarms' @::@ ['MetricAlarm']
--
-- * 'darNextToken' @::@ 'Maybe' 'Text'
--
describeAlarmsResponse :: DescribeAlarmsResponse
describeAlarmsResponse = DescribeAlarmsResponse
    { _darMetricAlarms = mempty
    , _darNextToken    = Nothing
    }

-- | A list of information for the specified alarms.
darMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darMetricAlarms = lens _darMetricAlarms (\s a -> s { _darMetricAlarms = a })

-- | A string that marks the start of the next batch of returned results.
darNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darNextToken = lens _darNextToken (\s a -> s { _darNextToken = a })

instance AWSRequest DescribeAlarms where
    type Sv DescribeAlarms = CloudWatch
    type Rs DescribeAlarms = DescribeAlarmsResponse

    request  = post "DescribeAlarms"
    response = xmlResponse $ \h x -> DescribeAlarmsResponse
        <$> x %| "MetricAlarms"
        <*> x %| "NextToken"
