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
    , mkDescribeAlarmsInput
    -- ** Request lenses
    , dajAlarmNames
    , dajAlarmNamePrefix
    , dajStateValue
    , dajActionPrefix
    , dajMaxRecords
    , dajNextToken

    -- * Response
    , DescribeAlarmsResponse
    -- ** Response lenses
    , daoMetricAlarms
    , daoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarms' request.
mkDescribeAlarmsInput :: DescribeAlarms
mkDescribeAlarmsInput = DescribeAlarms
    { _dajAlarmNames = mempty
    , _dajAlarmNamePrefix = Nothing
    , _dajStateValue = Nothing
    , _dajActionPrefix = Nothing
    , _dajMaxRecords = Nothing
    , _dajNextToken = Nothing
    }
{-# INLINE mkDescribeAlarmsInput #-}

data DescribeAlarms = DescribeAlarms
    { _dajAlarmNames :: [Text]
      -- ^ A list of alarm names to retrieve information for.
    , _dajAlarmNamePrefix :: Maybe Text
      -- ^ The alarm name prefix. AlarmNames cannot be specified if this
      -- parameter is specified.
    , _dajStateValue :: Maybe StateValue
      -- ^ The state value to be used in matching alarms.
    , _dajActionPrefix :: Maybe Text
      -- ^ The action name prefix.
    , _dajMaxRecords :: Maybe Integer
      -- ^ The maximum number of alarm descriptions to retrieve.
    , _dajNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Show, Generic)

-- | A list of alarm names to retrieve information for.
dajAlarmNames :: Lens' DescribeAlarms ([Text])
dajAlarmNames = lens _dajAlarmNames (\s a -> s { _dajAlarmNames = a })
{-# INLINE dajAlarmNames #-}

-- | The alarm name prefix. AlarmNames cannot be specified if this parameter is
-- specified.
dajAlarmNamePrefix :: Lens' DescribeAlarms (Maybe Text)
dajAlarmNamePrefix = lens _dajAlarmNamePrefix (\s a -> s { _dajAlarmNamePrefix = a })
{-# INLINE dajAlarmNamePrefix #-}

-- | The state value to be used in matching alarms.
dajStateValue :: Lens' DescribeAlarms (Maybe StateValue)
dajStateValue = lens _dajStateValue (\s a -> s { _dajStateValue = a })
{-# INLINE dajStateValue #-}

-- | The action name prefix.
dajActionPrefix :: Lens' DescribeAlarms (Maybe Text)
dajActionPrefix = lens _dajActionPrefix (\s a -> s { _dajActionPrefix = a })
{-# INLINE dajActionPrefix #-}

-- | The maximum number of alarm descriptions to retrieve.
dajMaxRecords :: Lens' DescribeAlarms (Maybe Integer)
dajMaxRecords = lens _dajMaxRecords (\s a -> s { _dajMaxRecords = a })
{-# INLINE dajMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dajNextToken :: Lens' DescribeAlarms (Maybe Text)
dajNextToken = lens _dajNextToken (\s a -> s { _dajNextToken = a })
{-# INLINE dajNextToken #-}

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
daoMetricAlarms = lens _daoMetricAlarms (\s a -> s { _daoMetricAlarms = a })
{-# INLINE daoMetricAlarms #-}

-- | A string that marks the start of the next batch of returned results.
daoNextToken :: Lens' DescribeAlarmsResponse (Maybe Text)
daoNextToken = lens _daoNextToken (\s a -> s { _daoNextToken = a })
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
