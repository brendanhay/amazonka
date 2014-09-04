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
    , describeAlarms
    -- ** Request lenses
    , dajActionPrefix
    , dajAlarmNamePrefix
    , dajAlarmNames
    , dajMaxRecords
    , dajNextToken
    , dajStateValue

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response lenses
    , daoMetricAlarms
    , daoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAlarms' request.
describeAlarms :: DescribeAlarms
describeAlarms = DescribeAlarms
    { _dajActionPrefix = Nothing
    , _dajAlarmNamePrefix = Nothing
    , _dajAlarmNames = mempty
    , _dajMaxRecords = Nothing
    , _dajNextToken = Nothing
    , _dajStateValue = Nothing
    }
{-# INLINE describeAlarms #-}

data DescribeAlarms = DescribeAlarms
    { _dajActionPrefix :: Maybe Text
      -- ^ The action name prefix.
    , _dajAlarmNamePrefix :: Maybe Text
      -- ^ The alarm name prefix. AlarmNames cannot be specified if this
      -- parameter is specified.
    , _dajAlarmNames :: [Text]
      -- ^ A list of alarm names to retrieve information for.
    , _dajMaxRecords :: Maybe Integer
      -- ^ The maximum number of alarm descriptions to retrieve.
    , _dajNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    , _dajStateValue :: Maybe StateValue
      -- ^ The state value to be used in matching alarms.
    } deriving (Show, Generic)

-- | The action name prefix.
dajActionPrefix :: Lens' DescribeAlarms (Maybe Text)
dajActionPrefix f x =
    f (_dajActionPrefix x)
        <&> \y -> x { _dajActionPrefix = y }
{-# INLINE dajActionPrefix #-}

-- | The alarm name prefix. AlarmNames cannot be specified if this parameter is
-- specified.
dajAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
dajAlarmNamePrefix f x =
    f (_dajAlarmNamePrefix x)
        <&> \y -> x { _dajAlarmNamePrefix = y }
{-# INLINE dajAlarmNamePrefix #-}

-- | A list of alarm names to retrieve information for.
dajAlarmNames :: Lens' DescribeAlarms ([Text])
dajAlarmNames f x =
    f (_dajAlarmNames x)
        <&> \y -> x { _dajAlarmNames = y }
{-# INLINE dajAlarmNames #-}

-- | The maximum number of alarm descriptions to retrieve.
dajMaxRecords :: Lens' DescribeAlarms (Maybe Integer)
dajMaxRecords f x =
    f (_dajMaxRecords x)
        <&> \y -> x { _dajMaxRecords = y }
{-# INLINE dajMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dajNextToken :: Lens' DescribeAlarms (Maybe Text)
dajNextToken f x =
    f (_dajNextToken x)
        <&> \y -> x { _dajNextToken = y }
{-# INLINE dajNextToken #-}

-- | The state value to be used in matching alarms.
dajStateValue :: Lens' DescribeAlarms (Maybe StateValue)
dajStateValue f x =
    f (_dajStateValue x)
        <&> \y -> x { _dajStateValue = y }
{-# INLINE dajStateValue #-}

instance ToQuery DescribeAlarms where
    toQuery = genericQuery def

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { _daoMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for the specified alarms.
    , _daoNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of information for the specified alarms.
daoMetricAlarms :: Lens' DescribeAlarmsResponse ([MetricAlarm])
daoMetricAlarms f x =
    f (_daoMetricAlarms x)
        <&> \y -> x { _daoMetricAlarms = y }
{-# INLINE daoMetricAlarms #-}

-- | A string that marks the start of the next batch of returned results.
daoNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
daoNextToken f x =
    f (_daoNextToken x)
        <&> \y -> x { _daoNextToken = y }
{-# INLINE daoNextToken #-}

instance FromXML DescribeAlarmsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarms where
    type Sv DescribeAlarms = CloudWatch
    type Rs DescribeAlarms = DescribeAlarmsResponse

    request = post "DescribeAlarms"
    response _ = xmlResponse

instance AWSPager DescribeAlarms where
    next rq rs = (\x -> rq { _dajNextToken = Just x })
        <$> (_daoNextToken rs)
