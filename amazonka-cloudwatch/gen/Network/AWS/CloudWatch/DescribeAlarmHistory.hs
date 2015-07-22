{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves history for the specified alarm. Filter alarms by date range
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
    , dahrqAlarmName
    , dahrqHistoryItemType
    , dahrqEndDate
    , dahrqStartDate
    , dahrqNextToken
    , dahrqMaxRecords

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response constructor
    , describeAlarmHistoryResponse
    -- ** Response lenses
    , dahrsAlarmHistoryItems
    , dahrsNextToken
    , dahrsStatus
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
-- * 'dahrqAlarmName'
--
-- * 'dahrqHistoryItemType'
--
-- * 'dahrqEndDate'
--
-- * 'dahrqStartDate'
--
-- * 'dahrqNextToken'
--
-- * 'dahrqMaxRecords'
data DescribeAlarmHistory = DescribeAlarmHistory'
    { _dahrqAlarmName       :: !(Maybe Text)
    , _dahrqHistoryItemType :: !(Maybe HistoryItemType)
    , _dahrqEndDate         :: !(Maybe ISO8601)
    , _dahrqStartDate       :: !(Maybe ISO8601)
    , _dahrqNextToken       :: !(Maybe Text)
    , _dahrqMaxRecords      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmHistory' smart constructor.
describeAlarmHistory :: DescribeAlarmHistory
describeAlarmHistory =
    DescribeAlarmHistory'
    { _dahrqAlarmName = Nothing
    , _dahrqHistoryItemType = Nothing
    , _dahrqEndDate = Nothing
    , _dahrqStartDate = Nothing
    , _dahrqNextToken = Nothing
    , _dahrqMaxRecords = Nothing
    }

-- | The name of the alarm.
dahrqAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahrqAlarmName = lens _dahrqAlarmName (\ s a -> s{_dahrqAlarmName = a});

-- | The type of alarm histories to retrieve.
dahrqHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahrqHistoryItemType = lens _dahrqHistoryItemType (\ s a -> s{_dahrqHistoryItemType = a});

-- | The ending date to retrieve alarm history.
dahrqEndDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahrqEndDate = lens _dahrqEndDate (\ s a -> s{_dahrqEndDate = a}) . mapping _Time;

-- | The starting date to retrieve alarm history.
dahrqStartDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahrqStartDate = lens _dahrqStartDate (\ s a -> s{_dahrqStartDate = a}) . mapping _Time;

-- | The token returned by a previous call to indicate that there is more
-- data available.
dahrqNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahrqNextToken = lens _dahrqNextToken (\ s a -> s{_dahrqNextToken = a});

-- | The maximum number of alarm history records to retrieve.
dahrqMaxRecords :: Lens' DescribeAlarmHistory (Maybe Natural)
dahrqMaxRecords = lens _dahrqMaxRecords (\ s a -> s{_dahrqMaxRecords = a}) . mapping _Nat;

instance AWSPager DescribeAlarmHistory where
        page rq rs
          | stop (rs ^. dahrsNextToken) = Nothing
          | stop (rs ^. dahrsAlarmHistoryItems) = Nothing
          | otherwise =
            Just $ rq & dahrqNextToken .~ rs ^. dahrsNextToken

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
               "AlarmName" =: _dahrqAlarmName,
               "HistoryItemType" =: _dahrqHistoryItemType,
               "EndDate" =: _dahrqEndDate,
               "StartDate" =: _dahrqStartDate,
               "NextToken" =: _dahrqNextToken,
               "MaxRecords" =: _dahrqMaxRecords]

-- | The output for the DescribeAlarmHistory action.
--
-- /See:/ 'describeAlarmHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dahrsAlarmHistoryItems'
--
-- * 'dahrsNextToken'
--
-- * 'dahrsStatus'
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
    { _dahrsAlarmHistoryItems :: !(Maybe [AlarmHistoryItem])
    , _dahrsNextToken         :: !(Maybe Text)
    , _dahrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAlarmHistoryResponse' smart constructor.
describeAlarmHistoryResponse :: Int -> DescribeAlarmHistoryResponse
describeAlarmHistoryResponse pStatus_ =
    DescribeAlarmHistoryResponse'
    { _dahrsAlarmHistoryItems = Nothing
    , _dahrsNextToken = Nothing
    , _dahrsStatus = pStatus_
    }

-- | A list of alarm histories in JSON format.
dahrsAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrsAlarmHistoryItems = lens _dahrsAlarmHistoryItems (\ s a -> s{_dahrsAlarmHistoryItems = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
dahrsNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrsNextToken = lens _dahrsNextToken (\ s a -> s{_dahrsNextToken = a});

-- | FIXME: Undocumented member.
dahrsStatus :: Lens' DescribeAlarmHistoryResponse Int
dahrsStatus = lens _dahrsStatus (\ s a -> s{_dahrsStatus = a});
