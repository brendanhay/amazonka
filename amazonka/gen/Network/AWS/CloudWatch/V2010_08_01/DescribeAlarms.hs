{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudWatch.V2010_08_01.DescribeAlarms where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAlarms' request.
describeAlarms :: DescribeAlarms
describeAlarms = DescribeAlarms
    { _daiActionPrefix = Nothing
    , _daiAlarmNamePrefix = Nothing
    , _daiAlarmNames = mempty
    , _daiMaxRecords = Nothing
    , _daiNextToken = Nothing
    , _daiStateValue = Nothing
    }

data DescribeAlarms = DescribeAlarms
    { _daiActionPrefix :: Maybe Text
      -- ^ The action name prefix.
    , _daiAlarmNamePrefix :: Maybe Text
      -- ^ The alarm name prefix. AlarmNames cannot be specified if this
      -- parameter is specified.
    , _daiAlarmNames :: [Text]
      -- ^ A list of alarm names to retrieve information for.
    , _daiMaxRecords :: Maybe Integer
      -- ^ The maximum number of alarm descriptions to retrieve.
    , _daiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    , _daiStateValue :: Maybe StateValue
      -- ^ The state value to be used in matching alarms.
    } deriving (Generic)

makeLenses ''DescribeAlarms

instance ToQuery DescribeAlarms where
    toQuery = genericToQuery def

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { _daoMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for the specified alarms.
    , _daoNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

makeLenses ''DescribeAlarmsResponse

instance FromXML DescribeAlarmsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarms where
    type Sv DescribeAlarms = CloudWatch
    type Rs DescribeAlarms = DescribeAlarmsResponse

    request = post "DescribeAlarms"
    response _ = xmlResponse

instance AWSPager DescribeAlarms where
    next rq rs = (\x -> rq { _daiNextToken = Just x })
        <$> (_daoNextToken rs)
