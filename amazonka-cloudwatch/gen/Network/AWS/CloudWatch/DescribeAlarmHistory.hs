{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves history for the specified alarm. Filter alarms by date range
-- or item type. If an alarm name is not specified, Amazon CloudWatch
-- returns histories for all of the owner\'s alarms.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html AWS API Reference> for DescribeAlarmHistory.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarmHistory
    (
    -- * Creating a Request
      describeAlarmHistory
    , DescribeAlarmHistory
    -- * Request Lenses
    , dahAlarmName
    , dahHistoryItemType
    , dahEndDate
    , dahStartDate
    , dahNextToken
    , dahMaxRecords

    -- * Destructuring the Response
    , describeAlarmHistoryResponse
    , DescribeAlarmHistoryResponse
    -- * Response Lenses
    , dahrsAlarmHistoryItems
    , dahrsNextToken
    , dahrsStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
    { _dahAlarmName       :: !(Maybe Text)
    , _dahHistoryItemType :: !(Maybe HistoryItemType)
    , _dahEndDate         :: !(Maybe ISO8601)
    , _dahStartDate       :: !(Maybe ISO8601)
    , _dahNextToken       :: !(Maybe Text)
    , _dahMaxRecords      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAlarmHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeAlarmHistory
    :: DescribeAlarmHistory
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
          | stop (rs ^. dahrsNextToken) = Nothing
          | stop (rs ^. dahrsAlarmHistoryItems) = Nothing
          | otherwise =
            Just $ rq & dahNextToken .~ rs ^. dahrsNextToken

instance AWSRequest DescribeAlarmHistory where
        type Rs DescribeAlarmHistory =
             DescribeAlarmHistoryResponse
        request = postQuery cloudWatch
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
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
    { _dahrsAlarmHistoryItems :: !(Maybe [AlarmHistoryItem])
    , _dahrsNextToken         :: !(Maybe Text)
    , _dahrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAlarmHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dahrsAlarmHistoryItems'
--
-- * 'dahrsNextToken'
--
-- * 'dahrsStatus'
describeAlarmHistoryResponse
    :: Int -- ^ 'dahrsStatus'
    -> DescribeAlarmHistoryResponse
describeAlarmHistoryResponse pStatus_ =
    DescribeAlarmHistoryResponse'
    { _dahrsAlarmHistoryItems = Nothing
    , _dahrsNextToken = Nothing
    , _dahrsStatus = pStatus_
    }

-- | A list of alarm histories in JSON format.
dahrsAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrsAlarmHistoryItems = lens _dahrsAlarmHistoryItems (\ s a -> s{_dahrsAlarmHistoryItems = a}) . _Default . _Coerce;

-- | A string that marks the start of the next batch of returned results.
dahrsNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrsNextToken = lens _dahrsNextToken (\ s a -> s{_dahrsNextToken = a});

-- | The response status code.
dahrsStatus :: Lens' DescribeAlarmHistoryResponse Int
dahrsStatus = lens _dahrsStatus (\ s a -> s{_dahrsStatus = a});
