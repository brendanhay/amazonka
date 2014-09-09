{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DescribeAlarms
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
module Network.AWS.CloudWatch.V2010_08_01.DescribeAlarms
    (
    -- * Request
      DescribeAlarms
    -- ** Request constructor
    , mkDescribeAlarms
    -- ** Request lenses
    , da1AlarmNames
    , da1AlarmNamePrefix
    , da1StateValue
    , da1ActionPrefix
    , da1MaxRecords
    , da1NextToken

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response constructor
    , mkDescribeAlarmsResponse
    -- ** Response lenses
    , darMetricAlarms
    , darNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

data DescribeAlarms = DescribeAlarms
    { _da1AlarmNames :: [Text]
    , _da1AlarmNamePrefix :: Maybe Text
    , _da1StateValue :: Maybe StateValue
    , _da1ActionPrefix :: Maybe Text
    , _da1MaxRecords :: Maybe Integer
    , _da1NextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarms' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AlarmNames ::@ @[Text]@
--
-- * @AlarmNamePrefix ::@ @Maybe Text@
--
-- * @StateValue ::@ @Maybe StateValue@
--
-- * @ActionPrefix ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeAlarms :: DescribeAlarms
mkDescribeAlarms = DescribeAlarms
    { _da1AlarmNames = mempty
    , _da1AlarmNamePrefix = Nothing
    , _da1StateValue = Nothing
    , _da1ActionPrefix = Nothing
    , _da1MaxRecords = Nothing
    , _da1NextToken = Nothing
    }

-- | A list of alarm names to retrieve information for.
da1AlarmNames :: Lens' DescribeAlarms [Text]
da1AlarmNames = lens _da1AlarmNames (\s a -> s { _da1AlarmNames = a })

-- | The alarm name prefix. AlarmNames cannot be specified if this parameter is
-- specified.
da1AlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
da1AlarmNamePrefix =
    lens _da1AlarmNamePrefix (\s a -> s { _da1AlarmNamePrefix = a })

-- | The state value to be used in matching alarms.
da1StateValue :: Lens' DescribeAlarms (Maybe StateValue)
da1StateValue = lens _da1StateValue (\s a -> s { _da1StateValue = a })

-- | The action name prefix.
da1ActionPrefix :: Lens' DescribeAlarms (Maybe Text)
da1ActionPrefix = lens _da1ActionPrefix (\s a -> s { _da1ActionPrefix = a })

-- | The maximum number of alarm descriptions to retrieve.
da1MaxRecords :: Lens' DescribeAlarms (Maybe Integer)
da1MaxRecords = lens _da1MaxRecords (\s a -> s { _da1MaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
da1NextToken :: Lens' DescribeAlarms (Maybe Text)
da1NextToken = lens _da1NextToken (\s a -> s { _da1NextToken = a })

instance ToQuery DescribeAlarms where
    toQuery = genericQuery def

-- | The output for the DescribeAlarms action.
data DescribeAlarmsResponse = DescribeAlarmsResponse
    { _darMetricAlarms :: [MetricAlarm]
    , _darNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MetricAlarms ::@ @[MetricAlarm]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeAlarmsResponse :: DescribeAlarmsResponse
mkDescribeAlarmsResponse = DescribeAlarmsResponse
    { _darMetricAlarms = mempty
    , _darNextToken = Nothing
    }

-- | A list of information for the specified alarms.
darMetricAlarms :: Lens' DescribeAlarmsResponse [MetricAlarm]
darMetricAlarms = lens _darMetricAlarms (\s a -> s { _darMetricAlarms = a })

-- | A string that marks the start of the next batch of returned results.
darNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
darNextToken = lens _darNextToken (\s a -> s { _darNextToken = a })

instance FromXML DescribeAlarmsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarms where
    type Sv DescribeAlarms = CloudWatch
    type Rs DescribeAlarms = DescribeAlarmsResponse

    request = post "DescribeAlarms"
    response _ = xmlResponse

instance AWSPager DescribeAlarms where
    next rq rs = (\x -> rq & da1NextToken ?~ x)
        <$> (rs ^. darNextToken)
