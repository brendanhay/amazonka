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

-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html>
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
    , _daAlarmNames      :: List "member" Text
    , _daMaxRecords      :: Maybe Nat
    , _daNextToken       :: Maybe Text
    , _daStateValue      :: Maybe StateValue
    } deriving (Eq, Read, Show)

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
-- * 'daStateValue' @::@ 'Maybe' 'StateValue'
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

-- | The alarm name prefix. 'AlarmNames' cannot be specified if this parameter is
-- specified.
daAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
daAlarmNamePrefix =
    lens _daAlarmNamePrefix (\s a -> s { _daAlarmNamePrefix = a })

-- | A list of alarm names to retrieve information for.
daAlarmNames :: Lens' DescribeAlarms [Text]
daAlarmNames = lens _daAlarmNames (\s a -> s { _daAlarmNames = a }) . _List

-- | The maximum number of alarm descriptions to retrieve.
daMaxRecords :: Lens' DescribeAlarms (Maybe Natural)
daMaxRecords = lens _daMaxRecords (\s a -> s { _daMaxRecords = a }) . mapping _Nat

-- | The token returned by a previous call to indicate that there is more data
-- available.
daNextToken :: Lens' DescribeAlarms (Maybe Text)
daNextToken = lens _daNextToken (\s a -> s { _daNextToken = a })

-- | The state value to be used in matching alarms.
daStateValue :: Lens' DescribeAlarms (Maybe StateValue)
daStateValue = lens _daStateValue (\s a -> s { _daStateValue = a })

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { _darMetricAlarms :: List "member" MetricAlarm
    , _darNextToken    :: Maybe Text
    } deriving (Eq, Read, Show)

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
darMetricAlarms = lens _darMetricAlarms (\s a -> s { _darMetricAlarms = a }) . _List

-- | A string that marks the start of the next batch of returned results.
darNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darNextToken = lens _darNextToken (\s a -> s { _darNextToken = a })

instance ToPath DescribeAlarms where
    toPath = const "/"

instance ToQuery DescribeAlarms where
    toQuery DescribeAlarms{..} = mconcat
        [ "ActionPrefix"    =? _daActionPrefix
        , "AlarmNamePrefix" =? _daAlarmNamePrefix
        , "AlarmNames"      =? _daAlarmNames
        , "MaxRecords"      =? _daMaxRecords
        , "NextToken"       =? _daNextToken
        , "StateValue"      =? _daStateValue
        ]

instance ToHeaders DescribeAlarms

instance AWSRequest DescribeAlarms where
    type Sv DescribeAlarms = CloudWatch
    type Rs DescribeAlarms = DescribeAlarmsResponse

    request  = post "DescribeAlarms"
    response = xmlResponse

instance FromXML DescribeAlarmsResponse where
    parseXML = withElement "DescribeAlarmsResult" $ \x -> DescribeAlarmsResponse
        <$> x .@? "MetricAlarms" .!@ mempty
        <*> x .@? "NextToken"

instance AWSPager DescribeAlarms where
    page rq rs
        | stop (rs ^. darNextToken) = Nothing
        | otherwise = (\x -> rq & daNextToken ?~ x)
            <$> (rs ^. darNextToken)
