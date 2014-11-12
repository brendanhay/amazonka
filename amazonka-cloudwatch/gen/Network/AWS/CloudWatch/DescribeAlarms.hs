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
      DescribeAlarmsInput
    -- ** Request constructor
    , describeAlarms
    -- ** Request lenses
    , dai1ActionPrefix
    , dai1AlarmNamePrefix
    , dai1AlarmNames
    , dai1MaxRecords
    , dai1NextToken
    , dai1StateValue

    -- * Response
    , DescribeAlarmsOutput
    -- ** Response constructor
    , describeAlarmsResponse
    -- ** Response lenses
    , daoMetricAlarms
    , daoNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data DescribeAlarmsInput = DescribeAlarmsInput
    { _dai1ActionPrefix    :: Maybe Text
    , _dai1AlarmNamePrefix :: Maybe Text
    , _dai1AlarmNames      :: [Text]
    , _dai1MaxRecords      :: Maybe Natural
    , _dai1NextToken       :: Maybe Text
    , _dai1StateValue      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAlarmsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dai1ActionPrefix' @::@ 'Maybe' 'Text'
--
-- * 'dai1AlarmNamePrefix' @::@ 'Maybe' 'Text'
--
-- * 'dai1AlarmNames' @::@ ['Text']
--
-- * 'dai1MaxRecords' @::@ 'Maybe' 'Natural'
--
-- * 'dai1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dai1StateValue' @::@ 'Maybe' 'Text'
--
describeAlarms :: DescribeAlarmsInput
describeAlarms = DescribeAlarmsInput
    { _dai1AlarmNames      = mempty
    , _dai1AlarmNamePrefix = Nothing
    , _dai1StateValue      = Nothing
    , _dai1ActionPrefix    = Nothing
    , _dai1MaxRecords      = Nothing
    , _dai1NextToken       = Nothing
    }

-- | The action name prefix.
dai1ActionPrefix :: Lens' DescribeAlarmsInput (Maybe Text)
dai1ActionPrefix = lens _dai1ActionPrefix (\s a -> s { _dai1ActionPrefix = a })

-- | The alarm name prefix. AlarmNames cannot be specified if this parameter
-- is specified.
dai1AlarmNamePrefix :: Lens' DescribeAlarmsInput (Maybe Text)
dai1AlarmNamePrefix =
    lens _dai1AlarmNamePrefix (\s a -> s { _dai1AlarmNamePrefix = a })

-- | A list of alarm names to retrieve information for.
dai1AlarmNames :: Lens' DescribeAlarmsInput [Text]
dai1AlarmNames = lens _dai1AlarmNames (\s a -> s { _dai1AlarmNames = a })

-- | The maximum number of alarm descriptions to retrieve.
dai1MaxRecords :: Lens' DescribeAlarmsInput (Maybe Natural)
dai1MaxRecords = lens _dai1MaxRecords (\s a -> s { _dai1MaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
dai1NextToken :: Lens' DescribeAlarmsInput (Maybe Text)
dai1NextToken = lens _dai1NextToken (\s a -> s { _dai1NextToken = a })

-- | The state value to be used in matching alarms.
dai1StateValue :: Lens' DescribeAlarmsInput (Maybe Text)
dai1StateValue = lens _dai1StateValue (\s a -> s { _dai1StateValue = a })

instance ToQuery DescribeAlarmsInput

instance ToPath DescribeAlarmsInput where
    toPath = const "/"

data DescribeAlarmsOutput = DescribeAlarmsOutput
    { _daoMetricAlarms :: [MetricAlarm]
    , _daoNextToken    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAlarmsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daoMetricAlarms' @::@ ['MetricAlarm']
--
-- * 'daoNextToken' @::@ 'Maybe' 'Text'
--
describeAlarmsResponse :: DescribeAlarmsOutput
describeAlarmsResponse = DescribeAlarmsOutput
    { _daoMetricAlarms = mempty
    , _daoNextToken    = Nothing
    }

-- | A list of information for the specified alarms.
daoMetricAlarms :: Lens' DescribeAlarmsOutput [MetricAlarm]
daoMetricAlarms = lens _daoMetricAlarms (\s a -> s { _daoMetricAlarms = a })

-- | A string that marks the start of the next batch of returned results.
daoNextToken :: Lens' DescribeAlarmsOutput (Maybe Text)
daoNextToken = lens _daoNextToken (\s a -> s { _daoNextToken = a })

instance FromXML DescribeAlarmsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAlarmsOutput"

instance AWSRequest DescribeAlarmsInput where
    type Sv DescribeAlarmsInput = CloudWatch
    type Rs DescribeAlarmsInput = DescribeAlarmsOutput

    request  = post "DescribeAlarms"
    response = xmlResponse $ \h x -> DescribeAlarmsOutput
        <$> x %| "MetricAlarms"
        <*> x %| "NextToken"
