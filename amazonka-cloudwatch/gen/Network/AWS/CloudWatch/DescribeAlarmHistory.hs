{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves history for the specified alarm. Filter alarms by date range
-- or item type. If an alarm name is not specified, Amazon CloudWatch
-- returns histories for all of the owner\'s alarms.
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
    , dahHistoryItemType
    , dahEndDate
    , dahStartDate
    , dahNextToken
    , dahMaxRecords

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response constructor
    , describeAlarmHistoryResponse
    -- ** Response lenses
    , dahrAlarmHistoryItems
    , dahrNextToken
    , dahrStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAlarmHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahAlarmName'
--
-- * 'dahHistoryItemType'
--
-- * 'dahEndDate'
--
-- * 'dahStartDate'
--
-- * 'dahNextToken'
--
-- * 'dahMaxRecords'
data DescribeAlarmHistory = DescribeAlarmHistory'
    { _dahAlarmName       :: !(Maybe Text)
    , _dahHistoryItemType :: !(Maybe HistoryItemType)
    , _dahEndDate         :: !(Maybe ISO8601)
    , _dahStartDate       :: !(Maybe ISO8601)
    , _dahNextToken       :: !(Maybe Text)
    , _dahMaxRecords      :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'DescribeAlarmHistory' smart constructor.
describeAlarmHistory :: DescribeAlarmHistory
describeAlarmHistory =
    DescribeAlarmHistory'
    { _dahAlarmName = Nothing
    , _dahHistoryItemType = Nothing
    , _dahEndDate = Nothing
    , _dahStartDate = Nothing
    , _dahNextToken = Nothing
    , _dahMaxRecords = Nothing
    }

-- | The name of the alarm.
dahAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahAlarmName = lens _dahAlarmName (\ s a -> s{_dahAlarmName = a});

-- | The type of alarm histories to retrieve.
dahHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahHistoryItemType = lens _dahHistoryItemType (\ s a -> s{_dahHistoryItemType = a});

-- | The ending date to retrieve alarm history.
dahEndDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahEndDate = lens _dahEndDate (\ s a -> s{_dahEndDate = a}) . mapping _Time;

-- | The starting date to retrieve alarm history.
dahStartDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahStartDate = lens _dahStartDate (\ s a -> s{_dahStartDate = a}) . mapping _Time;

-- | The token returned by a previous call to indicate that there is more
-- data available.
dahNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahNextToken = lens _dahNextToken (\ s a -> s{_dahNextToken = a});

-- | The maximum number of alarm history records to retrieve.
dahMaxRecords :: Lens' DescribeAlarmHistory (Maybe Natural)
dahMaxRecords = lens _dahMaxRecords (\ s a -> s{_dahMaxRecords = a}) . mapping _Nat;

instance AWSPager DescribeAlarmHistory where
        page rq rs
          | stop (rs ^. dahrNextToken) = Nothing
          | stop (rs ^. dahrAlarmHistoryItems) = Nothing
          | otherwise =
            Just $ rq & dahNextToken .~ rs ^. dahrNextToken

instance AWSRequest DescribeAlarmHistory where
        type Sv DescribeAlarmHistory = CloudWatch
        type Rs DescribeAlarmHistory =
             DescribeAlarmHistoryResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAlarmHistoryResult"
              (\ s h x ->
                 DescribeAlarmHistoryResponse' <$>
                   (x .@? "AlarmHistoryItems" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAlarmHistory where
        toHeaders = const mempty

instance ToPath DescribeAlarmHistory where
        toPath = const "/"

instance ToQuery DescribeAlarmHistory where
        toQuery DescribeAlarmHistory'{..}
          = mconcat
              ["Action" =: ("DescribeAlarmHistory" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmName" =: _dahAlarmName,
               "HistoryItemType" =: _dahHistoryItemType,
               "EndDate" =: _dahEndDate,
               "StartDate" =: _dahStartDate,
               "NextToken" =: _dahNextToken,
               "MaxRecords" =: _dahMaxRecords]

-- | The output for the DescribeAlarmHistory action.
--
-- /See:/ 'describeAlarmHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahrAlarmHistoryItems'
--
-- * 'dahrNextToken'
--
-- * 'dahrStatus'
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
    { _dahrAlarmHistoryItems :: !(Maybe [AlarmHistoryItem])
    , _dahrNextToken         :: !(Maybe Text)
    , _dahrStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeAlarmHistoryResponse' smart constructor.
describeAlarmHistoryResponse :: Int -> DescribeAlarmHistoryResponse
describeAlarmHistoryResponse pStatus =
    DescribeAlarmHistoryResponse'
    { _dahrAlarmHistoryItems = Nothing
    , _dahrNextToken = Nothing
    , _dahrStatus = pStatus
    }

-- | A list of alarm histories in JSON format.
dahrAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrAlarmHistoryItems = lens _dahrAlarmHistoryItems (\ s a -> s{_dahrAlarmHistoryItems = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
dahrNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrNextToken = lens _dahrNextToken (\ s a -> s{_dahrNextToken = a});

-- | FIXME: Undocumented member.
dahrStatus :: Lens' DescribeAlarmHistoryResponse Int
dahrStatus = lens _dahrStatus (\ s a -> s{_dahrStatus = a});
