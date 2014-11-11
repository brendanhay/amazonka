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

-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms.
module Network.AWS.CloudWatch.DescribeAlarmHistory
    (
    -- * Request
      DescribeAlarmHistoryInput
    -- ** Request constructor
    , describeAlarmHistoryInput
    -- ** Request lenses
    , dahiAlarmName
    , dahiEndDate
    , dahiHistoryItemType
    , dahiMaxRecords
    , dahiNextToken
    , dahiStartDate

    -- * Response
    , DescribeAlarmHistoryOutput
    -- ** Response constructor
    , describeAlarmHistoryOutput
    -- ** Response lenses
    , dahoAlarmHistoryItems
    , dahoNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data DescribeAlarmHistoryInput = DescribeAlarmHistoryInput
    { _dahiAlarmName       :: Maybe Text
    , _dahiEndDate         :: Maybe RFC822
    , _dahiHistoryItemType :: Maybe Text
    , _dahiMaxRecords      :: Maybe Int
    , _dahiNextToken       :: Maybe Text
    , _dahiStartDate       :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAlarmHistoryInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahiAlarmName' @::@ 'Maybe' 'Text'
--
-- * 'dahiEndDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dahiHistoryItemType' @::@ 'Maybe' 'Text'
--
-- * 'dahiMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dahiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dahiStartDate' @::@ 'Maybe' 'UTCTime'
--
describeAlarmHistoryInput :: DescribeAlarmHistoryInput
describeAlarmHistoryInput = DescribeAlarmHistoryInput
    { _dahiAlarmName       = Nothing
    , _dahiHistoryItemType = Nothing
    , _dahiStartDate       = Nothing
    , _dahiEndDate         = Nothing
    , _dahiMaxRecords      = Nothing
    , _dahiNextToken       = Nothing
    }

-- | The name of the alarm.
dahiAlarmName :: Lens' DescribeAlarmHistoryInput (Maybe Text)
dahiAlarmName = lens _dahiAlarmName (\s a -> s { _dahiAlarmName = a })

-- | The ending date to retrieve alarm history.
dahiEndDate :: Lens' DescribeAlarmHistoryInput (Maybe UTCTime)
dahiEndDate = lens _dahiEndDate (\s a -> s { _dahiEndDate = a })
    . mapping _Time

-- | The type of alarm histories to retrieve.
dahiHistoryItemType :: Lens' DescribeAlarmHistoryInput (Maybe Text)
dahiHistoryItemType =
    lens _dahiHistoryItemType (\s a -> s { _dahiHistoryItemType = a })

-- | The maximum number of alarm history records to retrieve.
dahiMaxRecords :: Lens' DescribeAlarmHistoryInput (Maybe Int)
dahiMaxRecords = lens _dahiMaxRecords (\s a -> s { _dahiMaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahiNextToken :: Lens' DescribeAlarmHistoryInput (Maybe Text)
dahiNextToken = lens _dahiNextToken (\s a -> s { _dahiNextToken = a })

-- | The starting date to retrieve alarm history.
dahiStartDate :: Lens' DescribeAlarmHistoryInput (Maybe UTCTime)
dahiStartDate = lens _dahiStartDate (\s a -> s { _dahiStartDate = a })
    . mapping _Time
instance ToQuery DescribeAlarmHistoryInput

instance ToPath DescribeAlarmHistoryInput where
    toPath = const "/"

data DescribeAlarmHistoryOutput = DescribeAlarmHistoryOutput
    { _dahoAlarmHistoryItems :: [AlarmHistoryItem]
    , _dahoNextToken         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAlarmHistoryOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahoAlarmHistoryItems' @::@ ['AlarmHistoryItem']
--
-- * 'dahoNextToken' @::@ 'Maybe' 'Text'
--
describeAlarmHistoryOutput :: DescribeAlarmHistoryOutput
describeAlarmHistoryOutput = DescribeAlarmHistoryOutput
    { _dahoAlarmHistoryItems = mempty
    , _dahoNextToken         = Nothing
    }

-- | A list of alarm histories in JSON format.
dahoAlarmHistoryItems :: Lens' DescribeAlarmHistoryOutput [AlarmHistoryItem]
dahoAlarmHistoryItems =
    lens _dahoAlarmHistoryItems (\s a -> s { _dahoAlarmHistoryItems = a })

-- | A string that marks the start of the next batch of returned results.
dahoNextToken :: Lens' DescribeAlarmHistoryOutput (Maybe Text)
dahoNextToken = lens _dahoNextToken (\s a -> s { _dahoNextToken = a })
instance FromXML DescribeAlarmHistoryOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAlarmHistoryOutput"

instance AWSRequest DescribeAlarmHistoryInput where
    type Sv DescribeAlarmHistoryInput = CloudWatch
    type Rs DescribeAlarmHistoryInput = DescribeAlarmHistoryOutput

    request  = post "DescribeAlarmHistory"
    response = xmlResponse $ \h x -> DescribeAlarmHistoryOutput
        <$> x %| "AlarmHistoryItems"
        <*> x %| "NextToken"
