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

-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
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

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html>
module Network.AWS.CloudWatch.DescribeAlarmHistory
    (
    -- * Request
      DescribeAlarmHistory
    -- ** Request constructor
    , describeAlarmHistory
    -- ** Request lenses
    , dahAlarmName
    , dahEndDate
    , dahHistoryItemType
    , dahMaxRecords
    , dahNextToken
    , dahStartDate

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response constructor
    , describeAlarmHistoryResponse
    -- ** Response lenses
    , dahrAlarmHistoryItems
    , dahrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data DescribeAlarmHistory = DescribeAlarmHistory
    { _dahAlarmName       :: Maybe Text
    , _dahEndDate         :: Maybe ISO8601
    , _dahHistoryItemType :: Maybe HistoryItemType
    , _dahMaxRecords      :: Maybe Nat
    , _dahNextToken       :: Maybe Text
    , _dahStartDate       :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'DescribeAlarmHistory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahAlarmName' @::@ 'Maybe' 'Text'
--
-- * 'dahEndDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dahHistoryItemType' @::@ 'Maybe' 'HistoryItemType'
--
-- * 'dahMaxRecords' @::@ 'Maybe' 'Natural'
--
-- * 'dahNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dahStartDate' @::@ 'Maybe' 'UTCTime'
--
describeAlarmHistory :: DescribeAlarmHistory
describeAlarmHistory = DescribeAlarmHistory
    { _dahAlarmName       = Nothing
    , _dahHistoryItemType = Nothing
    , _dahStartDate       = Nothing
    , _dahEndDate         = Nothing
    , _dahMaxRecords      = Nothing
    , _dahNextToken       = Nothing
    }

-- | The name of the alarm.
dahAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahAlarmName = lens _dahAlarmName (\s a -> s { _dahAlarmName = a })

-- | The ending date to retrieve alarm history.
dahEndDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahEndDate = lens _dahEndDate (\s a -> s { _dahEndDate = a }) . mapping _Time

-- | The type of alarm histories to retrieve.
dahHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahHistoryItemType =
    lens _dahHistoryItemType (\s a -> s { _dahHistoryItemType = a })

-- | The maximum number of alarm history records to retrieve.
dahMaxRecords :: Lens' DescribeAlarmHistory (Maybe Natural)
dahMaxRecords = lens _dahMaxRecords (\s a -> s { _dahMaxRecords = a }) . mapping _Nat

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahNextToken = lens _dahNextToken (\s a -> s { _dahNextToken = a })

-- | The starting date to retrieve alarm history.
dahStartDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahStartDate = lens _dahStartDate (\s a -> s { _dahStartDate = a }) . mapping _Time

data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { _dahrAlarmHistoryItems :: List "member" AlarmHistoryItem
    , _dahrNextToken         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeAlarmHistoryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahrAlarmHistoryItems' @::@ ['AlarmHistoryItem']
--
-- * 'dahrNextToken' @::@ 'Maybe' 'Text'
--
describeAlarmHistoryResponse :: DescribeAlarmHistoryResponse
describeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { _dahrAlarmHistoryItems = mempty
    , _dahrNextToken         = Nothing
    }

-- | A list of alarm histories in JSON format.
dahrAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrAlarmHistoryItems =
    lens _dahrAlarmHistoryItems (\s a -> s { _dahrAlarmHistoryItems = a })
        . _List

-- | A string that marks the start of the next batch of returned results.
dahrNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrNextToken = lens _dahrNextToken (\s a -> s { _dahrNextToken = a })

instance ToPath DescribeAlarmHistory where
    toPath = const "/"

instance ToQuery DescribeAlarmHistory where
    toQuery DescribeAlarmHistory{..} = mconcat
        [ "AlarmName"       =? _dahAlarmName
        , "EndDate"         =? _dahEndDate
        , "HistoryItemType" =? _dahHistoryItemType
        , "MaxRecords"      =? _dahMaxRecords
        , "NextToken"       =? _dahNextToken
        , "StartDate"       =? _dahStartDate
        ]

instance ToHeaders DescribeAlarmHistory

instance AWSRequest DescribeAlarmHistory where
    type Sv DescribeAlarmHistory = CloudWatch
    type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse

    request  = post "DescribeAlarmHistory"
    response = xmlResponse

instance FromXML DescribeAlarmHistoryResponse where
    parseXML = withElement "DescribeAlarmHistoryResult" $ \x -> DescribeAlarmHistoryResponse
        <$> x .@? "AlarmHistoryItems" .!@ mempty
        <*> x .@? "NextToken"

instance AWSPager DescribeAlarmHistory where
    page rq rs
        | stop (rs ^. dahrNextToken) = Nothing
        | otherwise = (\x -> rq & dahNextToken ?~ x)
            <$> (rs ^. dahrNextToken)
